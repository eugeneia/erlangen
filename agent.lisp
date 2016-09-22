;;;; Asynchronous agents (similar to Erlang processes).

(in-package :erlangen.agent)

(defstruct (agent (:constructor make-agent%))
  "*Syntax:*

   _agent_::= _structure_ | _keyword_ | _string_

   *Description:*

   An _agent_ can either be an _agent structure_, a _keyword_ denoting a
   registered _agent_ or a _string_ denoting a _remote agent_.

   A _remote agent_ is denoted by a _string_ of the form
   {\"}_host_{/}_node_{/}_agent_{\"} where _host_ is the host name,
   _node_ is the _node name_ and _agent_ is the _agent identifier_ of the
   _remote agent_.

   An _agent identifier_ is either a hexadecimal digit string denoting an
   _anonymous agent_ or a colon followed by a _symbol name_ denoting a
   _registered agent_. In the latter case, the _symbol name_ may not
   contain the slash ({/}) character.

   *Notes:*

   Only _agent structures_ are of _type_ {agent}."
  (mailbox (error "MAILBOX must be supplied.") :type mailbox)
  (links nil :type list)
  (monitors nil :type list)
  (lock (make-lock "erlangen.agent")))

(defmethod print-object ((o agent) stream)
  (print-unreadable-object (o stream :type t :identity t)))

(defmacro with-agent ((agent) &body body)
  "Lock AGENT for BODY."
  `(with-lock-grabbed ((agent-lock ,agent))
     ,@body))

(defvar *default-mailbox-size* 64
  "*Description:*

   {*default-mailbox-size*} is the default value of the {:mailbox-size}
   parameter to {spawn}.")

(defvar *agent-debug* nil
  "*Description:*

   If {*agent-debug*} is _true_ when calling {spawn}, _conditions_ of
   _type_ {serious-condition} will not be automatically handled for the
   spawned _agent_. The debugger will be entered so that the call stack
   can be inspected. Invoking the {exit} _restart_ will resume normal
   operation except that the exit reason will be the _agent_ instead of
   the fatal _condition_.")

(defvar *agent* (make-agent% :mailbox (make-mailbox *default-mailbox-size*))
  "Bound to current agent.")
;; *agent* will be rebound by the “real” agents, but for the initial processes
;; we create a “fake” agent structure with a mailbox. This way they can SPAWN,
;; SEND, and RECEIVE as if they were agents. See AGENT below.

(defun agent ()
  "*Description:*

   {agent} returns the _calling agent_."
  *agent*)

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
           (ignore-errors
             (erlangen:exit reason link)))
      ;; Message monitors.
      (loop for monitor in monitors do
           (ignore-errors
             (erlangen:send `(,*agent* . ,reason) monitor))))))

(defun exit (reason agent)
  "Node-local EXIT. See ERLANGEN:EXIT for generic implementation."
  (if (eq agent *agent*)
      ;; We are killing ourself: close our mailbox, then signal EXIT.
      (progn (close-mailbox (agent-mailbox *agent*))
             (error 'exit :reason reason))
      ;; We are killing another agent: send kill message, then close
      ;; agent's mailbox.
      (progn (send `(exit . ,reason) agent)
             (close-mailbox (agent-mailbox agent))))
  (values))

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

   If the _calling agent_ was killed by another _agent_ by use of {exit}
   a _serious-condition_ of _type_ {exit} is signaled.

   If _timeout_ is supplied and exceeded an _error_ of _type_ {timeout}
   is signaled."
  (flet ((receive-message ()
           (let ((message (dequeue-message (agent-mailbox *agent*))))
             (if (and (consp message) (eq 'exit (car message)))
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

(defun make-agent-process (function agent)
  "Spawn process for FUNCTION with *AGENT* bound to AGENT."
  (process-run-function "erlangen.agent" (make-agent-function function agent)))

(defun make-agent (function links monitors mailbox-size)
  "Create agent with LINKS, MONITORS and MAILBOX-SIZE that will execute
FUNCTION."
  (let ((agent (make-agent% :mailbox (make-mailbox mailbox-size)
                            :links links
                            :monitors monitors)))
    (make-agent-process function agent)
    agent))

(defun spawn-attached (mode to function mailbox-size)
  "Spawn agent with MAILBOX-SIZE that will execute FUNCTION attached to
TO in MODE."
  (let ((agent
         (ecase mode
           (:link
            (make-agent function (list to) nil mailbox-size))
           (:monitor
            (make-agent function nil (list to) mailbox-size)))))
    ;; Add link to TO only if its an AGENT structure.
    (typecase to
      (agent (with-agent (to)
               (push agent (agent-links to)))))
    agent))

(defun spawn (function &key attach
                            (to *agent*)
                            (mailbox-size *default-mailbox-size*))
  "Node-local SPAWN. See ERLANGEN:SPAWN for generic implementation."
  (ecase attach
    (:link     (spawn-attached :link to function mailbox-size))
    (:monitor  (spawn-attached :monitor to function mailbox-size))
    ((nil)     (make-agent function nil nil mailbox-size))))

(defun add-link (agent mode to)
  "Add link (TO) with MODE to AGENT."
  (with-agent (agent)
    (ecase mode
      (:link    (pushnew to (agent-links agent)    :test 'equal))
      (:monitor (pushnew to (agent-monitors agent) :test 'equal))))
  (values))

(defun remove-link (agent to)
  "Remove link (TO) from AGENT."
  (with-agent (agent)
    (setf #1=(agent-links agent)    (remove to #1# :test 'equal)
          #2=(agent-monitors agent) (remove to #2# :test 'equal)))
  (values))

(defun link (agent mode)
  "Node-local LINK. See ERLANGEN:LINK for generic implementation."
  (when (eq agent *agent*)
    (error "Can not link to self."))
  (add-link agent mode *agent*)
  (add-link *agent* :link agent))

(defun unlink (agent)
  "Node-local UNLINK. See ERLANGEN:UNLINK for generic implementation."
  (when (eq agent *agent*)
    (error "Can not unlink from self."))
  (remove-link agent *agent*)
  (remove-link *agent* agent))
