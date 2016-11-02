;;;; Erlangen public API.

(in-package :erlangen)

(defun send (message agent)
  "*Arguments and Values:*

   _message_—an _object_.

   _agent_—an _agent_.

   *Description*:

   {send} delivers _message_ to _agent_.

   *Exceptional Situations:*

   If _message_ could not be delivered successfully an _error_ of _type_
   {send-error} is signaled.

   If _agent_ is a _keyword_ that is not registered as a name an _error_ of
   _type_ {simple-error} is signaled."
  (etypecase agent
    (erlangen.agent:agent (erlangen.agent:send message agent))
    (keyword (erlangen.agent:send message (agent-by-name agent)))
    (string (remote-send message agent))))

(defun exit (&optional (reason :kill) (agent (agent)))
  "*Arguments and Values:*

   _reason_—an _object_.

   _agent_—an _agent_. Default is the _calling agent_.

   *Description*:

   {exit} kills _agent_ with _reason_ as the _exit reason_. Subsequent attempts
   to send messages to _agent_ will fail. If _agent_ is the _calling agent_ it
   exits immediately, otherwise {exit} delivers an _exit message_ to _agent_.

   *Exceptional Situations:*

   If _agent_ is a _keyword_ that is not registered as a name an _error_ of
   _type_ {simple-error} is signaled."
  (etypecase agent
    (erlangen.agent:agent (erlangen.agent:exit reason agent))
    (keyword (erlangen.agent:exit reason (agent-by-name agent)))
    (string (remote-exit reason agent))))

(defun notify (exited reason agent)
  "Notify AGENT of the exit REASON of EXITED. This in a solely internal hook
for ERLANGEN.AGENT."
  (etypecase agent
    (erlangen.agent:agent (erlangen.agent:notify exited reason agent))
    (keyword (erlangen.agent:notify exited reason (agent-by-name agent)))
    (string (remote-notify (agent-id exited) reason agent))))

(defun spawn (function &key attach (mailbox-size *default-mailbox-size*)
                            node (host (host-name)))
  "*Arguments and Values:*

   _function_—a _function designator_ or a _call_.

   _attach_—either {:link}, {:attach}, or {nil}. The default is {nil}.

   _mailbox-size_—a positive _unsigned integer_. The default is
   {*default-mailbox-size*}.

   _node_—a _node name_ or {nil}. The default is {nil}.

   _host_—a _host_ as accepted by [resolve-address](http://ccl.clozure.com/docs/ccl.html#f_resolve-address).
   The default is the local host name as reported by {machine-instance}.

   *Description:*

   {spawn} starts and returns a new _agent_ with a mailbox capacity of
   _mailbox-size_. If _attach_ is {:link} or {:monitor} the _calling
   agent_ will be linked to the new _agent_ as if by {link} but before
   the _agent_ is started. Once the _agent_ is started it will execute
   _function_.

   If _node_ is _non-nil_ the _agent_ is started on _node_ of _host_ instead of
   the local node.

   *Exceptional Situations:*

   If {spawn} fails to start the _agent_ an an _error_ of _type_ {error} is
   signaled."
  (if (null node)
      (erlangen.agent:spawn (etypecase function
                              ((or function symbol) function)
                              (call (make-function function)))
                            :attach attach
                            :mailbox-size mailbox-size)
      (let ((agent (remote-spawn host
                                 node
                                 (if (symbolp function)
                                     (list function)
                                     function)
                                 (and attach (agent-id (agent)))
                                 attach
                                 mailbox-size)))
        (when attach
          (erlangen.agent:link agent attach))
        agent)))

(defun link (agent &optional (mode :link))
  "*Arguments and Values:*

   _agent_—an _agent_.

   _mode_—either {:link} or {:monitor}. The default is {:link}.

   *Description*:

   {link} _links_ the _calling agent_ to _agent_. After two _agents_ are
   _linked_ they behave as follows:

   When the _calling agent_ exits it will attempt to kill _agent_ with
   the _exit reason_ of the _calling agent_.

   When _agent_ exits, and _mode_ is {:link} it will attempt to kill the
   _calling agent_ with the _exit reason_ of _agent_.

   When _agent_ exits, and _mode_ is {:monitor} it will attempt to send an
   _exit notification_ to the _calling agent_.

   An _exit notification_ is of the form

   {(} _agent_ _status_ . _values_ {)}

   _status_::= {:ok} | {:exit}

   The _status_ {:ok} indicates that the _agent_ exited normally, and
   _values_ will be a list of its _return values_.

   The _status_ {:exit} indicates that the _agent_ was either killed by
   {exit} or aborted because of an unhandled _condition_ of _type_
   {serious-condition}, and _values_ will be the _exit reason_ supplied to
   {exit}, or the _condition object_.

   *Exceptional Situations:*

   If _agent_ is the _calling agent_ an _error_ of _type_ {simple-error}
   is signaled.

   If _agent_ is a _keyword_ that is not registered as a name an _error_ of
   _type_ {simple-error} is signaled."
  (etypecase agent
    (erlangen.agent:agent (erlangen.agent:link agent mode))
    (keyword (erlangen.agent:link (agent-by-name agent) mode))
    (string (erlangen.agent:link agent mode)
            (remote-link agent (agent-id (agent)) mode))))


(defun unlink (agent)
  "*Arguments and Values:*

   _agent_—an _agent_.

   *Description:*

   {unlink} removes any _link_ between _agent_ and the _calling agent_.

   *Exceptional Situations:*

   If _agent_ is the _calling agent_ an _error_ of _type_ {simple-error}
   is signaled.

   If _agent_ is a _keyword_ that is not registered as a name an _error_ of
   _type_ {simple-error} is signaled."
  (etypecase agent
    (erlangen.agent:agent (erlangen.agent:unlink agent))
    (keyword (erlangen.agent:unlink (agent-by-name agent)))
    (string (erlangen.agent:unlink agent)
            (remote-unlink agent (agent-id (agent))))))

(defun node (&key (host "localhost") name)
  "*Arguments and Values:*

   _host_—a _host_ as accepted by [resolve-address](http://ccl.clozure.com/docs/ccl.html#f_resolve-address).
   The default is {\"localhost\"}.

   _name_—a _string_. The default is a unique name.

   *Description:*

   {node} spawns the node protocol server to listen on a random free port
   of _host_. It then registers its _name_ and listening port with the
   port mapper. Once the node is registered, it is capable of
   communicating with remote nodes.

   *Examples:*

   #code#
   ;; Start talking to remote nodes:
   (spawn '(node))
   #

   *Exceptional Situations:*

   If _name_ can not be registered (e.g., because it has already been
   registered by another node, or because the port mapper is unreachable)
   an _error_ of _type_ {error} is signaled, and the node protocol server
   is killed."
  (register :node)
  (when name
    (setf (node-name) name))
  (unwind-protect (multiple-value-bind (node-server port)
                      (make-node-server :host host)
                    (spawn node-server :attach :link)
                    (register-node (node-name) port))
    (clear-connections)))
