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
  "Returns hostname."
  (machine-instance))

(defun gen-node-name ()
  "Returns node name generated from Unix pid."
  (format nil "node-~a" (ccl::getpid)))

(defvar *node-name*/lock (make-read-write-lock))
(defvar *node-name* (gen-node-name)
  "Node name.")

(defun node-name ()
  "Returns the name (a string) of this node."
  (with-read-lock (*node-name*/lock)
    *node-name*))

(defun set-node-name (name)
  "Sets the node name to NAME."
  (check-type name name)
  (with-write-lock (*node-name*/lock)
    (setf *node-name* name)))

(defsetf node-name set-node-name)

(defvar *agent-counter*/lock (make-lock "…id::*agent-counter*"))
(defvar *agent-counter* 0
  "Counter used for agent id generation.")

(defun gen-aid ()
  "Generates and returns unique identifier for node-local agent."
  (with-lock-grabbed (*agent-counter*/lock)
    (prog1 (format nil "~x" *agent-counter*)
      (incf *agent-counter*))))

(defun generated-aid-p (aid)
  "Predicate to test if AID has been generated."
  (< (parse-integer aid :radix 16 :junk-allowed nil)
     (with-lock-grabbed (*agent-counter*/lock)
       *agent-counter*)))

(deftype aid ()
  "Type of generated agent IDs."
  '(and string (satisfies generated-aid-p)))

(defvar *agent<->aid*/lock (make-read-write-lock))
(defvar *agent->aid* (make-hash-table :test 'eq :weak :key)
  "Agent to aid mapping.")
(defvar *aid->agent* (make-hash-table :test 'equal :weak :value)
  "Aid to agent mapping.")

(defun bind-aid (aid agent)
  "Associate AID with AGENT."
  (with-write-lock (*agent<->aid*/lock)
    (setf (gethash aid *aid->agent*) agent
          (gethash agent *agent->aid*) aid)))

(defun intern-anonymous-aid (agent)
  "If possible, returns the existing aid for AGENT. Otherwise a new aid
for AGENT is created and returned."
  (or (with-read-lock (*agent<->aid*/lock)
        (gethash agent *agent->aid*))
      (bind-aid (gen-aid) agent)))

(defun find-anonymous-agent (aid)
  "Returns agent by AID."
  (with-read-lock (*agent<->aid*/lock)
    (gethash aid *aid->agent*)))

(defun registry-aid (name)
  "Returns aid for agent registered for NAME. Signals an error if no aid
could be derived from NAME."
  (let ((aid (prin1-to-string name)))
    (check-type aid name)
    aid))

(defun find-registered-agent (aid)
  "Returns registered agent by AID or nil if no such agent exists."
  (ignore-errors (agent-by-name (find-symbol aid :keyword))))

(defun agent-id (&optional agent)
  "Return id for AGENT. If AGENT is nil return free id."
  (format nil "~a/~a/~a"
          (host-name)
          (node-name)
          (etypecase agent
            (agent   (intern-anonymous-aid agent))
            (keyword (registry-aid agent))
            (null    (gen-aid)))))

(defun decode-id (id)
  "Decodes unique agent ID. Returns host name, node name, and aid."
  (values-list (split-sequence #\/ id)))

(defun decode-aid (aid)
  "Decodes AID and returns agent id type (:REGISTERED or :ANONYMOUS) and
name."
  (if (string= ":" aid :end2 1)
      (values :registered (subseq aid 1))
      (values :anonymous  aid)))

(defun find-agent (id)
  "Returns agent by ID."
  (multiple-value-bind (host node aid) (decode-id id)
    (when (and (equal host (host-name))
               (equal node (node-name)))
      (multiple-value-bind (type aid) (decode-aid aid)
        (values (ecase type
                  (:anonymous  (find-anonymous-agent aid))
                  (:registered (find-registered-agent aid))))))))

(defun claim-id (id agent)
  "Claim ID for AGENT."
  (multiple-value-bind (host node aid) (decode-id id)
    (assert (equal host (host-name)))
    (assert (equal node (node-name)))
    (check-type aid aid)
    (assert (null (find-anonymous-agent aid)))
    (bind-aid aid agent)))

;; Specialized cl-conspack encoding/decoding for AGENT structures. Values
;; of type AGENT will be encoded as their respective agent id and decoded
;; as plain strings.
(defmethod encode-object ((agent agent) &key &allow-other-keys)
  `((,(agent-id agent))))
(defmethod decode-object ((class (eql 'agent)) alist
                          &key &allow-other-keys)
  (caar alist))
