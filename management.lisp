(in-package :erlangen.management)

(defgeneric root (agent-tree)
  (:documentation
   "→ _agent_

    *Arguments and Values:*

    _agent-tree_—an _object_ of _type_ {agent-tree}.

    _agent_—an _agent_.

    *Description*:

    {root} returns the _agent_ that is the root of _agent-tree_."))

(defgeneric linked (agent-tree)
  (:documentation
   "→ _list_

    *Arguments and Values:*

    _agent-tree_—an _object_ of _type_ {agent-tree}.

    _list_—a _list_ of _agents_.

    *Description*:

    {linked} returns a _list_ of _agents_ that are linked to but not monitored
    by the root _agent_ of _agent-tree_."))

(defgeneric monitored (agent-tree)
  (:documentation
   "→ _subtrees_

    *Arguments and Values:*

    _agent-tree_—an _object_ of _type_ {agent-tree}.

    _subtrees_—a _list_ of _objects_ of _type_ {agent-tree}.

    *Description*:

    {monitored} returns a _list_ of the _subtrees_ whose root _agents_ are
    monitored by the root _agent_ of _agent-tree_."))

(defclass agent-tree ()
  ((root :initarg :root :reader root)
   (linked :initarg :linked :reader linked-to)
   (monitored :initarg :monitored :reader monitored-by))
  (:documentation
   "*Description:*

    Instances of class {agent-tree} denote views of the _agent_ graph at a
    specific point in time. Their {print-object} method prints an elaborate
    description of that view when {*print-readably*} is {nil}."))

(defun agent-tree (agent)
  "→ _agent-tree_

   *Arguments and Values:*

   _agent_—an _agent_.

   _agent-tree_—an _instance_ of _class_ {agent-tree}.

   *Description:*

   {agent-tree} returns the current _agent-tree_ whose root is _agent_."
  (let (linked monitored)
    (typecase agent
      (erlangen.agent:agent ; stop at local edges
       (loop for x in (agent-links agent)
          if (and ; remote agents show up as links
                  (typep x 'erlangen.agent:agent)
                  (find agent (agent-monitors x)))
          do
            (push (agent-tree x) monitored)
          else do
            (push x linked))))
    (make-instance 'agent-tree
                   :root agent
                   :linked linked
                   :monitored monitored)))

(defvar *print-agent-tree-indent* 0)
(defmethod print-object ((o agent-tree) stream)
  (if *print-readably*
      (call-next-method)
      (with-slots (root linked monitored) o
        (format stream "~v@{~A~:*~}~*~:[↳~;~]~a~@[ (linked: ~{~a~^ ~})~]~%"
                #1=*print-agent-tree-indent* #\Space (= 0 #1#)
                root linked)
        (let ((#1# (+ #1# 4)))
          (loop for m in monitored do (print-object m stream))))))

(defun process-agent (process &key (timeout 1))
  "→ _agent_

   *Arguments and Values:*

   _process_—a _process_.

   _timeout_—a positive _integer_ denoting a time interval in seconds. The
   default is 1.

   _agent_—an _agent_ or {nil}.

   *Description:*

   {process-agent} interrupts _process_ to retrieve its associated _agent_. It
   returns the respective _agent_ or {nil}. A return value of {nil} indicates
   that _process_ could not be interrupted within the duration specified by
   _timeout_."
  (let (agent)
    (process-interrupt
     process (lambda () (setf agent (agent))))
    (process-wait-with-timeout
     "…management:process-agent" timeout (lambda () agent))))

(defun flush-messages (&key (print-p t) (stream *standard-output*))
  "*Arguments and Values:*

   _print-p_—a _generalized boolean_. The default is _true_.

   _stream_—an _output stream_. The default is {*standard-output*}.

   *Description:*

   {flush-messages} dequeues messages from the mailbox of the _calling agent_
   until there are no more pending messages. If _print-p_ is _true_ each
   dequeued message is printed to _stream_."
  (ignore-errors
    (loop for message = (erlangen:receive :timeout 0)
       when print-p do (print message stream)))
  (values))
