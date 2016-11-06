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

(defvar *aid-counter*/lock (make-lock "…id::*aid-counter*"))
(defvar *aid-counter* 0
  "Counter used for agent id generation.")

(defun gen-aid ()
  "Generates and returns unique identifier for node-local agent."
  (format nil "~x" (with-lock-grabbed (*aid-counter*/lock)
                     (prog1 *aid-counter*
                       (incf *aid-counter*)))))

(defun aid-value (aid)
  "Return numerical value for AID."
  (parse-integer aid :radix 16 :junk-allowed nil))

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

(defun format-id (aid)
  "Return id for AID."
  (format nil "~a/~a/~a" (host-name) (node-name) aid))

(defun agent-id (agent)
  "Return id for AGENT."
  (format-id (etypecase agent
               (agent   (intern-anonymous-aid agent))
               (keyword (registry-aid agent)))))

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

(defvar *reserved*/lock (make-lock "…id::*reserved-ids*"))
(defvar *reserved* nil
  "List of `aid' reservations")


(defun reserve-id ()
  "Reserve a free id."
  (let* ((aid (gen-aid))
         (n (aid-value aid)))
    (with-lock-grabbed (*reserved*/lock)
      (push n *reserved*))
    (format-id aid)))

(defun claim-id (id)
  "Claim reserved ID. Removes ID from the list of reserved ids, and return T if
ID was removed and NIL otherwise. At the same, CLAIM-ID acts as a sort of
garbace collector. Heuristically stale reservations are removed from the list
of reserved ids, and if a stale reservation is claimed an error is signaled."
  (multiple-value-bind (host node aid) (decode-id id)
    (assert (equal host (host-name)))
    (assert (equal node (node-name)))
    (let ((n (aid-value aid))
          (found-p nil))
      (labels ((stale-p (n)
                 (< (+ n 1000000) *aid-counter*))
               (delete-p (reserved)
                 (cond ((= reserved n) (setf found-p t))
                       ((stale-p reserved) t))))
        (with-lock-grabbed (*reserved*/lock)
          (setf *reserved* (delete-if #'delete-p *reserved*)))
        (assert (not (stale-p n))))
      found-p)))

(defun bind-id (id agent)
  "Bind aid in ID to AGENT."
  (multiple-value-bind (host node aid) (decode-id id)
    (declare (ignore host node))
    (bind-aid aid agent)))

;; Specialized cl-conspack encoding/decoding for AGENT structures. Values
;; of type AGENT will be encoded as their respective agent id and decoded
;; as plain strings.
(defmethod encode-object ((agent agent) &key &allow-other-keys)
  `((,(agent-id agent))))
(defmethod decode-object ((class (eql 'agent)) alist
                          &key &allow-other-keys)
  (caar alist))
