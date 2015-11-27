;;;; Erlangen universal agent identifiers.

(in-package :erlangen.distribution.id)

(defun valid-name-p (string)
  "Predicate to test if STRING is a valid name (e.g. not empty and does
not contain #\/)."
  (not (or (= (length string) 0) (find #\/ string))))

(deftype name ()
  "Type for host, node and agent names."
  '(and string (satisfies valid-name-p)))

(defun host-name ()
  "Get hostname string."
  (machine-instance))

(defun gen-node-name ()
  "Generate node name from pid."
  (format nil "node-~a" (ccl::getpid)))

(defvar *node-name*/lock (make-read-write-lock))
(defvar *node-name* (gen-node-name)
  "Node name.")

(defun node-name ()
  "Return the name (a string) of this node."
  (with-read-lock (*node-name*/lock)
    *node-name*))

(defun set-node-name (name)
  "Set *NODE-NAME* to NAME."
  (check-type name string)
  (with-write-lock (*node-name*/lock)
    (setf *node-name* name)))

(defsetf node-name set-node-name)

(defvar *agent-counter*/lock (make-read-write-lock))
(defvar *agent-counter* 0
  "Counter used for agent id generation.")

(defun gen-aid ()
  "Generate unique identifier for node-local agent."
  (prog1 (format nil "~x" (with-read-lock (*agent-counter*/lock)
                            *agent-counter*))
    (with-write-lock (*agent-counter*/lock)
      (incf *agent-counter*))))

(defun decode-id (id)
  "Decode uniqe agent ID."
  (apply 'values (split-sequence #\/ id)))

(defvar *agent<->aid*/lock (make-lock))
(defvar *agent->aid* (make-hash-table :test 'eq :weak :key)
  "Agent to aid mapping.")
(defvar *aid->agent* (make-hash-table :test 'equal :weak :value)
  "Aid to agent mapping.")

(defun intern-anonymous-aid (agent)
  (with-lock-grabbed (*agent<->aid*/lock)
    (or (gethash agent *agent->aid*)
        (let ((aid (gen-aid)))
          (setf (gethash aid *aid->agent*) agent
                (gethash agent *agent->aid*) aid)))))

(defun find-anonymous-agent (aid)
  (with-lock-grabbed (*agent<->aid*/lock)
    (gethash aid *aid->agent*)))

(defun registry-aid (keyword)
  (let ((name (prin1-to-string keyword)))
    (if (valid-name-p name)
        name
        (error "~a is not a valid agent id." name))))

(defun agent-id (agent)
  "Return id for AGENT."
  (format nil "~a/~a/~a"
          (host-name)
          (node-name)
          (etypecase agent
            (agent   (intern-anonymous-aid agent))
            (keyword (registry-aid agent)))))

(defun decode-aid (aid)
  (if (string= ":" aid :end2 1)
      (values :registered (subseq aid 1))
      (values :anonymous  aid)))

(defun find-agent (id)
  "Return agent for ID."
  (multiple-value-bind (host node aid) (decode-id id)
    (when (and (string= host (host-name))
               (string= node (node-name)))
      (multiple-value-bind (type aid) (decode-aid aid)
        (values (ecase type
                  (:anonymous  (find-anonymous-agent aid))
                  (:registered (find-symbol aid :keyword))))))))
