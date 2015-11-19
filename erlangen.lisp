;;;; Erlangen public API.

(defpackage erlangen
  (:documentation
   "Distributed asychronous message passing system for Common Lisp.")
  (:use :cl
        :erlangen.agent
        :erlangen.registry
        :erlangen.conditions
        :erlangen.macros
        :erlangen.distribution.call)
  ;; Some symbols with generic definitions (local/registered/remote
  ;; agents) are redefined in this package and thus shadowed.
  (:shadow :send
           :exit
           :link
           :unlink
           :spawn)
  (:export :agent
           :spawn
           :call
           :link
           :unlink
           :send
           :send-error
           :receive
           :timeout
           :exit
           :register
           :unregister
           :registered
           :select
           :*default-mailbox-size*
           :*agent-debug*))
     
(in-package :erlangen)

(defun send (message agent)
  "*Arguments and Values:*

   _message_—an _object_.

   _agent_—an _agent_.

   *Description*:

   {send} delivers _message_ to _agent_.

   *Exceptional Situations:*

   If _message_ could not be delivered successfully an _error_ of _type_
   {send-error} is signaled."
  (etypecase agent
    (agent (erlangen.agent:send message agent))
    (symbol (erlangen.agent:send message (agent-by-name agent)))))

(defun exit (&optional (reason :kill) (agent (agent)))
  "*Arguments and Values:*

   _reason_—an _object_.

   _agent_—an _agent_. Default is the _calling agent_.

   *Description*:

   {exit} kills _agent_ with _reason_ as the _exit reason_ of _agent_.

   *Exceptional Situations:*

   If _agent_ is not the _calling agent_ and the _kill message_ could not
   be delivered successfully an _error_ of _type_ {send-error} is
   signaled."
  (etypecase agent
    (agent (erlangen.agent:exit reason agent))
    (symbol (erlangen.agent:exit reason (agent-by-name agent)))))

(defun spawn (function &key attach (mailbox-size *default-mailbox-size*))
  "*Arguments and Values:*

   _function_—a _function designator_ or a _call_.

   _attach_—a _keyword_ or {nil}.

   _mailbox-size_—a positive _unsigned integer_. The default is
   {*default-mailbox-size*}.

   *Description:*

   {spawn} starts and returns a new _agent_ with a mailbox capacity of
   _mailbox-size_. If _attach_ is {:link} or {:monitor} the _calling
   agent_ will be linked to the new _agent_ as if by {link} but before
   the _agent_ is started. Once the _agent_ is started it will execute
   _function_.

   *Exceptional Situations:*

   If _attach_ is {:link} or {:monitor} and {spawn} was not called by an
   _agent_ an _error_ of _type_ {type-error} is signaled."
  (erlangen.agent:spawn (etypecase function
                          (function function)
                          (call (make-function function)))
                        :attach attach
                        :mailbox-size mailbox-size))

(defun link (agent &optional (mode :link))
  "*Arguments and Values:*

   _agent_—an _agent_.

   _mode_—a _keyword_. Either {:link} or {:monitor}. Defaults to {:link}.

   *Description*:

   {link} _links_ the _calling agent_ to _agent_. After two _agents_ are
   _linked_ they behave as follows:

   When the _calling agent_ exits it will attempt to kill _agent_ with
   the _exit reason_ of the _calling agent_.

   When _agent_ exits and _mode_ is {:link} it will attempt to kill
   _calling agent_ with the _exit reason_ of _agent_.

   When _agent_ exits and _mode_ is {:monitor} it will attempt to send an
   _exit notification_ to the _calling agent_.

   An _exit notification_ is of the form

   {(} _agent_ {.} _exit-reason_ {)}

   where _exit-reason_ is the _exit reason_ of _agent_.

   An _exit reason_ has the following from:

   {(} _status_ {.} _values_ {)}

   A _status_ of {:ok} means that the _agent_ exited normally and
   _values_ will be a list of its _return values_.

   A _status_ of {:exit} means that the _agent_ was either killed by
   {exit} or aborted because of an unhandled _condition_ of _type_
   {serious-condition} and _values_ will be the _reason_ supplied to
   {exit} or the _condition object_.

   The attempts to kill or notify _linked agents_ will fail if the
   respective message can not be delivered to the target _agent_. Any
   error conditions that arise due to the failure will be silently
   ignored.

   *Exceptional Situations:*

   If {link} was not called by an _agent_ an _error_ of _type_
   {type-error} is signaled.

   If _agent_ is the _calling agent_ an _error_ of _type_ {simple-error}
   is signaled."
  (etypecase agent
    (agent (erlangen.agent:link agent mode))
    (symbol (erlangen.agent:link (agent-by-name agent) mode))))

(defun unlink (agent)
  "*Arguments and Values:*

   _agent_—an _agent_.

   *Description:*

   {unlink} removes any _link_ between _agent_ and the _calling agent_.

   *Exceptional Situations:*

   If {unlink} was not called by an _agent_ an _error_ of _type_
   {type-error} is signaled.

   If _agent_ is the _calling agent_ an _error_ of _type_ {simple-error}
   is signaled."
  (etypecase agent
    (agent (erlangen.agent:unlink agent))
    (symbol (erlangen.agent:unlink (agent-by-name agent)))))
