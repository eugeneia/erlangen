;;;; Asynchronous agents (similar to Erlang processes).

(defpackage erlangen.agent
  (:documentation
   "Node-local implementation of asynchronous _agents_ that can
  communicate via message passing. The interface is mostly equivalent to
  Erlang's processes except (1) without asynchronous exceptions and (2)
  without a blocking version of {send}.")
  (:use :cl
        :erlangen.mailbox
        :erlangen.conditions
        :erlangen.algorithms
        :bordeaux-threads)
  (:shadowing-import-from :erlangen.conditions :timeout)
  (:export :*agent*
           :agent
           :spawn
           :link
           :unlink
           :send
           :receive
           :exit
           :*default-mailbox-size*
           :*agent-debug*))

(in-package :erlangen.agent)

(defvar *agent* nil
  "Bound to current agent.")

(defvar *default-mailbox-size* 64
  "*Description:*

   {*default-mailbox-size*} is the default value of the {:mailbox-size}
   parameter to {spawn}.")

(defparameter *agent-debug* nil
  "*Description:*

   If {*agent-debug*} is _true_ when calling {spawn} _conditions_ of
   _type_ {serious-condition} will not be automatically handled for the
   spawned _agent_. The debugger will be entered so that the call stack
   can be inspected. Invoking the {exit} _restart_ will resume normal
   operation except that the exit reason will be the _agent_ instead of
   the fatal _condition_.")

(defun agent ()
  "Return calling agent."
  *agent*)

(defstruct (agent (:constructor make-agent%))
  "Agent structure."
  (mailbox (error "MAILBOX must be supplied.") :type mailbox)
  (links nil :type list)
  (monitors nil :type list)
  (lock (make-lock "erlangen.agent-lock") :type lock))

(defmethod print-object ((o agent) stream)
  (print-unreadable-object (o stream :type t :identity t)))

(defmacro with-agent ((agent) &body body)
  "Lock AGENT for BODY."
  `(with-lock-held ((agent-lock ,agent))
     ,@body))

(define-condition exit (serious-condition)
  ((reason
    :initform (error "Must supply REASON.")
    :initarg :reason
    :reader exit-reason
    :documentation "Reson for exit."))
  (:documentation
   "*Description:*

    Describes a condition that denotes the exit of the _calling
    agent_. The function {exit-reason} can be used to retrieve the exit
    reason"))

(defun send (message agent)
  "Node-local SEND. See ERLANGEN:SEND for generic implementation."
  (handler-case (enqueue-message message (agent-mailbox agent))
    (error (error)
      (declare (ignore error))
      (error 'send-error)))
  (values))

(defun agent-notify-exit (reason)
  "Kill links and message monitors of *AGENT* due to exit for REASON."
  (with-slots (links monitors lock) *agent*
    (with-agent (*agent*)
      ;; Kill links.
      (loop for link in links do
           (ignore-errors (exit reason link)))
      ;; Message monitors.
      (loop for monitor in monitors do
           (ignore-errors (send `(,*agent* . ,reason) monitor))))))

(defun exit (reason agent)
  "Node-local EXIT. See ERLANGEN:EXIT for generic implementation."
  (if (eq agent *agent*)
      ;; We are killing ourself: close our mailbox, then signal EXIT.
      (progn (close-mailbox (agent-mailbox *agent*))
             (error 'exit :reason reason))
      ;; We are killing another agent: send kill message, then close
      ;; agent's mailbox.
      (progn (send `(:exit . ,reason) agent)
             (close-mailbox (agent-mailbox agent)))))

(defun receive (&key timeout (poll-interval 1e-3))
  "*Arguments and Values:*

   _timeout_, _poll-interval_—positive _numbers_.

   *Description*:

   {receive} returns the next message for the _calling agent_. If the
   _mailbox_ of the _calling agent_ is empty, {receive} will block until
   a message arrives.

   If _timeout_ is supplied {receive} will block for at most _timeout_
   seconds and poll for a message every _poll-interval_ seconds.

   *Exceptional Situations:*

   If {receive} is not called by an _agent_ an _error_ of _type_
   {type-error} is signaled.

   If the _calling agent_ was killed by another _agent_ by use of {exit}
   a _serious-condition_ of _type_ {exit} is signaled.

   If _timeout_ is supplied and exceeded and _error_ of _type_ {timeout}
   is signaled."
  (check-type *agent* agent)
  (flet ((receive-message ()
           (let ((message (dequeue-message (agent-mailbox *agent*))))
             (if (and (consp message) (eq :exit (car message)))
                 (error 'exit :reason (cdr message))
                 message))))
    (if timeout
        (with-poll-timeout ((not (empty-p (agent-mailbox *agent*)))
                            :timeout timeout
                            :poll-interval poll-interval)
          :succeed (receive-message)
          :fail (error 'timeout))
        (receive-message))))

(defun make-agent-function (function agent)
  "Wrap FUNCTION in ordered shutdown forms for AGENT."
  (let ((default-mailbox-size *default-mailbox-size*)
        (debug-p *agent-debug*))
    (flet ((run-agent ()
             (handler-case (funcall function)
               ;; Agent exits normally.
               (:no-error (&rest values)
                 (agent-notify-exit `(:ok . ,values)))
               ;; Agent is killed with reason.
               (exit (exit)
                 (agent-notify-exit `(:exit . ,(exit-reason exit))))))
           ;; Handler for when agent signals a SERIOUS-CONDITION.
           (handle-agent-error (condition)
             ;; Unless DEBUG-P is true the EXIT restart
             ;; is invoked automatically.
             (unless debug-p
               (invoke-restart 'exit condition)))
           ;; Report and interactive functions for EXIT restart.
           (exit-report (stream) (format stream "Exit ~a." *agent*))
           (exit-interactive () `(,agent)))
      (lambda ()
        ;; Pass on relevant dynamic variables to child agent.
        (let ((*agent* agent)
              (*default-mailbox-size* default-mailbox-size)
              (*agent-debug* debug-p))
          ;; We run agent and set up restarts and handlers for its
          ;; failure modes. RUN-AGENT handles normal exits itself. If
          ;; agent signals a SERIOUS-CONDITION however, the failure is
          ;; handled seperately (and possibly interactively, if
          ;; *AGENT-DEBUG* is true).
          (restart-case
              (handler-bind ((serious-condition #'handle-agent-error))
                (run-agent))
            ;; Restart for when agent signals a SERIOUS-CONDITION.
            (exit (condition)
              :report exit-report :interactive exit-interactive
              (agent-notify-exit `(:exit . ,condition)))))))))

(defun make-agent-thread (function agent)
  "Spawn thread for FUNCTION with *AGENT* bound to AGENT."
  (make-thread (make-agent-function function agent)
               :name "erlangen.agent"))

(defun make-agent (function links monitors mailbox-size)
  "Create agent with LINKS, MONITORS and MAILBOX-SIZE that will execute
FUNCTION."
  (let ((agent (make-agent% :mailbox (make-mailbox mailbox-size)
                            :links links
                            :monitors monitors)))
    (make-agent-thread function agent)
    agent))

(defun spawn-attached (mode function mailbox-size)
  "Spawn agent with MAILBOX-SIZE that will execute FUNCTION attached to
the calling agent in MODE."
  (let ((agent
         (ecase mode
           (:link
            (make-agent function (list *agent*) nil mailbox-size))
           (:monitor
            (make-agent function nil (list *agent*) mailbox-size)))))
    (with-agent (*agent*)
      (push agent (agent-links *agent*)))
    agent))

(defun spawn (function &key attach (mailbox-size *default-mailbox-size*))
  "*Arguments and Values:*

   _function_—a _function_.

   _attach_—a _keyword_ or {nil}.

   _mailbox-size_—a positive _unsigned integer_.

   *Description:*

   {spawn} starts and returns a new _agent_ with a mailbox capacity of
   _mailbox-size_. If _attach_ is {:link} or {:monitor} the _calling
   agent_ will be linked to the new _agent_ as if by {link} but before
   the _agent_ is started.

   *Exceptional Situations:*

   If _attach_ is {:link} or {:monitor} and {spawn} was not called by an
   _agent_ an _error_ of _type_ {type-error} is signaled."
  (case attach
    (:link     (check-type *agent* agent)
               (spawn-attached :link function mailbox-size))
    (:monitor  (check-type *agent* agent)
               (spawn-attached :monitor function mailbox-size))
    (otherwise (make-agent function nil nil mailbox-size))))

(defun link (agent mode)
  "Node-local LINK. See ERLANGEN:LINK for generic implementation."
  (check-type *agent* agent)
  (when (eq agent *agent*)
    (error "Can not link to self."))
  (with-agent (agent)
    (ecase mode
      (:link    (pushnew *agent* (agent-links agent)))
      (:monitor (pushnew *agent* (agent-monitors agent)))))
  (with-agent (*agent*)
    (pushnew agent (agent-links *agent*))))

(defun unlink (agent)
  "Node-local UNLINK. See ERLANGEN:UNLINK for generic implementation."
  (check-type *agent* agent)
  (when (eq agent *agent*)
    (error "Can not unlink from self."))
  (with-agent (agent)
    (setf #1=(agent-links agent)    (remove *agent* #1#)
          #2=(agent-monitors agent) (remove *agent* #2#)))
  (with-agent (*agent*)
    (setf #3=(agent-links *agent*)    (remove agent #3#)
          #4=(agent-monitors *agent*) (remove agent #4#))))
