;;;; Named agent registry.

(in-package :erlangen.registry)

(defvar *registry* (make-hash-table :lock-free t))

(defvar *registry*/lock (make-read-write-lock))

(defun register (name &optional (agent (agent)))
  "*Arguments and Values:*

   _name_—a _keyword_.

   _agent_—an _agent_. Default is the _calling agent_.

   *Description*:

   {register} associates _name_ with _agent_.

   *Exceptional Situations:*

   If _name_ is already associated with an _agent_ an _error_ of _type_
   {simple-error} is signaled."
  (check-type name keyword)
  (check-type agent agent)
  (with-write-lock (*registry*/lock)
    (if #1=(gethash name *registry*)
        (error "~a is already registered." name)
        (setf #1# agent))))

(defun unregister (name)
  "*Arguments and Values:*

   _name_—a _keyword_.

   *Description*:

   {unregister} removes the registered _name_, associated with an
   _agent_.

   *Exceptional Situations:*

   If the _name_ is not associated with an _agent_ an _error_ of _type_
   {simple-error} is signaled."
  (check-type name keyword)
  (with-write-lock (*registry*/lock)
    (or (remhash name *registry*)
        (error "Not registered: ~a" name))))

(defun registered ()
  "*Description*:

   {registered} returns a _list_ of names associated with _agents_."
  (with-read-lock (*registry*/lock)
    (loop for name being the hash-keys of *registry* collect name)))

(defun agent-by-name (name)
  "Return agent by NAME if registered, signal error otherwise."
  (with-read-lock (*registry*/lock)
    (or (gethash name *registry*)
        (error "No such agent: ~a" name))))
