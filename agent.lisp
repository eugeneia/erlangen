;;;; Asynchronous agents (similar to Erlang processes).

(defpackage erlangen.agent
  (:documentation
   "Node-local implementation of asynchronous _agents_ that can
  communicate via message passing. The interface is mostly equivalent to
  Erlang's processes except (1) without asynchronous exceptions and (2)
  without a blocking version of {send}.")
  (:use :cl
        :erlangen.mailbox
        :bordeaux-threads)
  (:export :agent
           :spawn
           :link
           :unlink
           :send
           :send-error
           :receive
           :exit))

(in-package :erlangen.agent)

(defvar *agent* nil
  "Bound to current agent.")

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

(define-condition send-error (error) ()
  (:documentation
   "*Description:*

    Describes an error condition that can occur when calling {send}. It
    denotes a that {send} was unable to successfully deliver the message
    to the recepient."))

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

   If {receive} was not called by an _agent_ an _error_ of _type_
   {type-error} is signaled.

   If the _calling agent_ was killed by another _agent_ by use of {exit}
   a _serious-condition_ of _type_ {exit} is signaled.

   If _timeout_ was supplied and is exceeded and _error_ of _type_
   {simple-error} is signaled."
  (check-type *agent* agent)
  (flet ((receive-message ()
           (let ((message (dequeue-message (agent-mailbox *agent*))))
             (if (and (consp message) (eq :exit (car message)))
                 (error 'exit :reason (cdr message))
                 message))))
    (if timeout
        (loop for elapsed = 0 then (+ elapsed poll-interval) do
             (when (> elapsed timeout)
               (error "Timout in RECEIVE."))
             (unless (empty-p (agent-mailbox *agent*))
               (return-from receive (receive-message)))
             (sleep poll-interval))
        (receive-message))))

(defun make-agent-thread (function agent)
  "Spawn thread for FUNCTION with *AGENT* bound to AGENT."
  (make-thread (lambda ()
                 (let ((*agent* agent))
                   (handler-case (funcall function)
                     ;; Agent exits normally.
                     (:no-error (&rest values)
                       (agent-notify-exit `(:ok . ,values)))
                     ;; Agent is killed with reason.
                     (exit (exit)
                       (agent-notify-exit `(:exit . ,(exit-reason exit))))
                     ;; Agent exits due to SERIOUS-CONDITION.
                     (serious-condition (condition)
                       (agent-notify-exit `(:exit . ,condition))))))
               :name "erlangen.agent"))

(defun make-agent (function links monitors mailbox-size)
  "Create agent with LINKS, MONITORS and MAILBOX-SIZE that will execute
FUNCTION."
  (let ((agent (make-agent% :mailbox (make-mailbox mailbox-size)
                            :links links
                            :monitors monitors)))
    (make-agent-thread function agent)
    agent))

(defun spawn (function &key attach (mailbox-size 64))
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
               (make-agent function (list *agent*) nil mailbox-size))
    (:monitor  (check-type *agent* agent)
               (make-agent function nil (list *agent*) mailbox-size))
    (otherwise (make-agent function nil nil mailbox-size))))

(defun link (agent mode)
  "Node-local LINK. See ERLANGEN:LINK for generic implementation."
  (with-agent (agent)
    (check-type *agent* agent)
    (ecase mode
      (:link    (pushnew *agent* (agent-links agent)))
      (:monitor (pushnew *agent* (agent-monitors agent))))))

(defun unlink (agent)
  "Node-local UNLINK. See ERLANGEN:UNLINK for generic implementation."
  (with-agent (agent)
    (check-type *agent* agent)
    (setf #1=(agent-links agent)    (remove *agent* #1#)
          #2=(agent-monitors agent) (remove *agent* #2#))))
