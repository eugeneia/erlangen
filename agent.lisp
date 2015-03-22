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
  "*Arguments and Values:*

   _message_—an _object_.

   _agent_—an _agent_.

   *Description*:

   {send} delivers _message_ to _agent_.

   *Exceptional Situations:*

   If the *message* could not be delivered successfully an _error_ of
   _type_ {send-error} is signaled."
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

(defun exit (&optional (reason :kill) (agent *agent*))
  "*Arguments and Values:*

   _reason_—an _object_.

   _agent_—an _agent_. Default is the _calling agent_.

   *Description*:

   {exit} kills _agent_ with _reason_ as the _exit reason_ of _agent_.

   *Exceptional Situations:*

   If _agent_ is not the _calling agent_ and the _kill message_ could not
   be delivered successfully an _error_ of _type_ {send-error} is
   signaled."
  (if (eq agent *agent*)
      (error 'exit :reason reason)
      (send `(:exit . ,reason) agent)))

(defun receive ()
  "*Description*:

   {receive} returns the next message for the _calling agent_. If the
   _mailbox_ of the _calling agent_ is empty, {receive} will block until
   a message arrives.

   *Exceptional Situations:*

   If {receive} was not called by an _agent_ an _error_ of _type_
   {type-error} is signaled.

   If the _calling agent_ was killed by another _agent_ by use of {exit}
   a _serious-condition_ of _type_ {exit} is signaled."
  (check-type *agent* agent)
  (let ((message (dequeue-message (agent-mailbox *agent*))))
    (if (and (consp message) (eq :exit (car message)))
        (error 'exit :reason (cdr message))
        message)))

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

(defun link (agent &optional (mode :link))
  "*Arguments and Values:*

   _agent_—an _agent_.

   _mode_—a _keyword_. Either {:link} or {:monitor}. Defaults to {:link}.

   *Description*:

   {link} _links_ the _calling agent_ to _agent_.

   When _agent_ exits and _mode_ is {:link} the _calling agent_ will be
   killed with the _exit reason_ of _agent_.

   When _agent_ exits and _mode_ is {:monitor} the _calling agent_ will
   receive a message of the form

   #code#
   (<agent> . <exit-reason>)
   #

   where _exit-reason_ is the _exit reason_ of _agent_.

   An _exit reason_ has the following from:

   {(} _status_ {.} _values_ {)}

   A _status_ of {:ok} means that the _agent_ exited normally and
   _values_ will be a list of its _return values_.

   A _status_ of {:exit} means that the _agent_ was either killed by
   {exit} or aborted because of an unhandled _condition_ of _type_
   {serious-condition} and _values_ will be the _reason_ supplied to
   {exit} or the _condition object_.

   *Exceptional Situations:*

   If {link} was not called by an _agent_ an _error_ of _type_
   {type-error} is signaled."
  (with-agent (agent)
    (check-type *agent* agent)
    (ecase mode
      (:link    (pushnew *agent* (agent-links agent)))
      (:monitor (pushnew *agent* (agent-monitors agent))))))

(defun unlink (agent)
  "*Arguments and Values:*

   _agent_—an _agent_.

   *Description:*

   {unlink} removes any _links_ from _agent_ to the _calling agent_.

   *Exceptional Situations:*

   If {unlink} was not called by an _agent_ an _error_ of _type_
   {type-error} is signaled."
  (with-agent (agent)
    (check-type *agent* agent)
    (setf #1=(agent-links agent)    (remove *agent* #1#)
          #2=(agent-monitors agent) (remove *agent* #2#))))
