;;;; Erlangen public API.

(defpackage erlangen
  (:documentation
   "Distributed asychronous message passing system for Common Lisp.")
  (:use :cl
        :erlangen.agent
        :erlangen.registry
        :erlangen.conditions)
  ;; Some symbols with generic definitions (local/registered/remote
  ;; agents) are redefined in this package and thus shadowed.
  (:shadow :send
           :exit
           :link
           :unlink)
  (:export :agent
           :spawn
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

(defun link (agent &optional (mode :link))
  "*Arguments and Values:*

   _agent_—an _agent_.

   _mode_—a _keyword_. Either {:link} or {:monitor}. Defaults to {:link}.

   *Description*:

   {link} _links_ the _calling agent_ to _agent_.

   When the _calling agent_ exits _agent_ will be killed with the _exit
   reason_ of the _calling agent_.

   When _agent_ exits and _mode_ is {:link} the _calling agent_ will be
   killed with the _exit reason_ of _agent_.

   When _agent_ exits and _mode_ is {:monitor} the _calling agent_ will
   receive a message of the form

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

   {unlink} removes any _links_ from _agent_ to the _calling agent_.

   *Exceptional Situations:*

   If {unlink} was not called by an _agent_ an _error_ of _type_
   {type-error} is signaled.

   If _agent_ is the _calling agent_ an _error_ of _type_ {simple-error}
   is signaled."
  (etypecase agent
    (agent (erlangen.agent:unlink agent))
    (symbol (erlangen.agent:unlink (agent-by-name agent)))))
