;;;; Asynchronous agents (similar to Erlang processes).

(in-package :erlangen.agent)

(defclass agent ()
  ((mailbox :initarg :mailbox
            :initform (error "missing :MAILBOX argument to MAKE-INSTANCE of AGENT.")
            :reader agent-mailbox
            :type mailbox)
   (links :initarg :links
          :initform nil
          :accessor agent-links
          :type list)
   (monitors :initarg :monitors
             :initform nil
             :accessor agent-monitors
             :type list)
   (lock :initform (make-lock "erlangen.agent")
         :reader agent-lock)
   (symbol :initarg :symbol
           :initform nil
           :accessor agent-symbol
           :type symbol)
   (birthtime :initform (get-universal-time)
              :reader agent-birthtime
              :type (integer 0))
   (deathtime :initform 0
              :accessor agent-deathtime
              :type (integer 0)))
  (:documentation 
  "*Syntax:*

   _agent_::= _instance_ | _keyword_ | _string_

   *Description:*

   An _agent_ can either be an _agent instance_, a _keyword_ denoting a
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

   Only _agent instances_ are of _type_ {agent}."))

(defmethod print-object ((o agent) stream)
  (if (agent-symbol o)
      (print-unreadable-object (o stream :type t :identity t)
        (let ((*package* (find-package :keyword)))
          (prin1 (agent-symbol o) stream)))
      (print-unreadable-object (o stream :type t :identity t))))

(defun agent-stats (agent)
  "→ _messages-received_, _messages-dropped_, _birthtime_, _deathtime_

   *Arguments and Values:*

   _agent_—an _agent_.

   _messages-received_—a positive _integer_ denoting the number of messages
   received by _agent_.

   _messages-dropped_—a positive _integer_ denoting the number of messages
   dropped by _agent_ because its mailbox was full.

   _birthtime_—a _universal time_ denoting the time when _agent_ was started.

   _deathtime_—a _universal time_ denoting the time when _agent_ exited, or
   {nil} if _agent_ has not exited.

   *Description:*

   {agent-stats} returns various current statistics for _agent_."
  (with-slots (mailbox birthtime deathtime) agent
    (values (mailbox-messages-dequeued mailbox)
            (mailbox-messages-dropped mailbox)
            birthtime
            (when (> deathtime 0)
              deathtime))))

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

(defvar *agent* (make-instance 'agent :mailbox (make-mailbox *default-mailbox-size*))
  "Bound to current agent.")
;; *agent* will be rebound by the “real” agents, but for the initial processes
;; we create a “fake” agent instance with a mailbox. This way they can SPAWN,
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
    :documentation "Reason for EXIT."))
  (:documentation
   "Conditions of type `exit' are signaled by agents when they EXIT."))

(defmethod print-object ((o exit) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (prin1 (exit-reason o) stream)))

(defun send (message agent)
  "Node-local SEND. See ERLANGEN:SEND for generic implementation."
  (enqueue-message message (agent-mailbox agent))
  (values))

(defun exit (reason agent)
  "Node-local EXIT. See ERLANGEN:EXIT for generic implementation."
  (if (eq agent *agent*)
      ;; We are killing ourself: signal EXIT.
      (error 'exit :reason reason)
      ;; We are killing another agent: enqueue EXIT message, then close
      ;; agent's mailbox.
      (progn (enqueue-priority `(exit . ,reason) (agent-mailbox agent))
             (close-mailbox (agent-mailbox agent))))
  (values))

(defun receive (&key timeout)
  "*Arguments and Values:*

   _timeout_—a positive _number_ denoting a time interval in seconds.

   *Description*:

   {receive} returns the next message for the _calling agent_. If the message
   is an _exit message_ the _calling agent_ exits immediately. If the _mailbox_
   of the _calling agent_ is empty, {receive} will block until a message
   arrives.

   If _timeout_ is supplied {receive} will block for at most _timeout_ seconds.

   *Exceptional Situations:*

   If _timeout_ is supplied and the specified time interval exceeded an _error_
   of _type_ {timeout} is signaled."
  (let ((message (dequeue-message (agent-mailbox *agent*) :timeout timeout)))
    (if (and (consp message) (eq 'exit (car message)))
        (error 'exit :reason (cdr message))
        message)))

(defun add-link (agent mode to)
  "Add link (TO) with MODE to AGENT."
  (with-agent (agent)
    (ecase mode
      (:link    (pushnew to (agent-links agent)    :test 'equal))
      (:monitor (pushnew to (agent-monitors agent) :test 'equal)))))

(defun remove-link (agent to &aux removed-p)
  "Remove link (TO) from AGENT."
  (flet ((equal! (x y)
           (when (equal x y)
             (setf removed-p t))))
    (with-agent (agent)
      (setf #1=(agent-links agent)    (remove to #1# :test #'equal!)
            #2=(agent-monitors agent) (remove to #2# :test #'equal!))))
  removed-p)

(defun link (agent mode)
  "Node-local LINK. See ERLANGEN:LINK for generic implementation."
  (when (eq agent *agent*)
    (error "Can not link to self."))
  (typecase agent
    (agent (add-link agent mode *agent*)))
  (add-link *agent* :link agent)
  (values))

(defun unlink (agent)
  "Node-local UNLINK. See ERLANGEN:UNLINK for generic implementation."
  (when (eq agent *agent*)
    (error "Can not unlink from self."))
  (typecase agent
    (agent (remove-link agent *agent*)))
  (remove-link *agent* agent)
  (values))

(defun notify (exited reason agent)
  "Node-local NOTIFY. See ERLANGEN:NOTIFY for generic implementation."
  (when (remove-link agent exited)
    (enqueue-priority `(,exited . ,reason) (agent-mailbox agent)))
  (values))

(defun agent-notify-exit (reason)
  "Kill links and message monitors of *AGENT* due to exit for REASON."
  (with-slots (links monitors lock) *agent*
    (with-agent (*agent*)
      ;; Kill links.
      (loop for link in links do
           (erlangen:exit reason link))
      ;; Message monitors.
      (loop for monitor in monitors do
           (erlangen::notify *agent* reason monitor)))))

(defun make-agent-function (function agent)
  "Wrap FUNCTION in ordered shutdown forms for AGENT."
  (let ((default-mailbox-size *default-mailbox-size*)
        (debug-p *agent-debug*))
    (flet ((run-agent ()
             (handler-case (unwind-protect (funcall function)
                             (close-mailbox (agent-mailbox agent))
                             (setf (agent-deathtime agent)
                                   (get-universal-time)))
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

(defun spawn (function
              &key
                attach
                (to *agent*)
                (mailbox-size *default-mailbox-size*)
                (class 'agent)
                agent-symbol
                instance-args)
  "Node-local SPAWN. See ERLANGEN:SPAWN for generic implementation."
  (check-type attach (member :link :monitor nil))
  (check-type to (or null agent))       ; :TO must be actual agent (not registered name)
  (let ((agent (apply 'make-instance class
                      :mailbox (make-mailbox mailbox-size)
                      :links (when (eql attach :link)
                               (list to))
                      :monitors (when (eql attach :monitor)
                                  (list to))
                      :symbol agent-symbol
                      :allow-other-keys t
                      instance-args)))
    (when to
      (with-agent (to)
        (push agent (agent-links to))))
    (process-run-function "erlangen.agent" (make-agent-function function agent))
    agent))
