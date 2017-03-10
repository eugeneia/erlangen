(defpackage erlangen-platform.mesh-table
  (:use :cl :ccl :erlangen :erlangen-platform.log)
  (:shadow :node)
  (:export :*key-size* :*replication* :*timeout* :*response-backlog*
           :node :client :gen-id :route
           :make-get-request :make-put-request :make-delete-request
           :get-reply-value :reply-sequence))

(in-package :erlangen-platform.mesh-table)

(defparameter *key-size* 128)
(defparameter *replication* 4)
(defparameter *timeout* (* 2 60 internal-time-units-per-second))
(defparameter *response-backlog* 100)

(defstruct (route (:constructor route (id agent)))
  id agent (ctime (get-internal-real-time)))

(defstruct (bucket (:constructor bucket (&optional (bound *key-size*))))
  bound (free *replication*) routes)

(defstruct node
  id (buckets (list (bucket))) values (requests (ring *response-backlog*)))

(defun distance (x y)
  (logxor x y))

(defun bucket-key-p (bucket distance)
  (< distance (expt 2 (bucket-bound bucket))))

(defun find-bucket (key node)
  (find-if (let ((distance (distance key (node-id node))))
             (lambda (bucket)
               (bucket-key-p bucket distance)))
           (node-buckets node)))

(defun bucket-add (bucket route)
  (push route (bucket-routes bucket))
  (decf (bucket-free bucket)))

(defun bucket-delete (bucket route)
  (setf #1=(bucket-routes bucket) (delete route #1#))
  (incf (bucket-free bucket)))

(defun route-stale-p (route)
  (> (- (get-internal-real-time) (route-ctime route))
     *timeout*))

(defun bucket-delete-stale (bucket)
  (let ((stale-route (find-if 'route-stale-p (bucket-routes bucket))))
    (when stale-route
      (bucket-delete bucket stale-route))))

(defun split-bucket (node)
  (let* ((bucket (first (node-buckets node)))
         (new (bucket (1- (bucket-bound bucket)))))
    (dolist (route (bucket-routes bucket))
      (when (bucket-key-p new (distance (route-id route) (node-id node)))
        (bucket-delete bucket route)
        (bucket-add new route)))
    (push new (node-buckets node))))

(defun add-route (node route)
  (let ((bucket (find-bucket (route-id route) node)))
    (cond ((or (plusp (bucket-free bucket))
               (bucket-delete-stale bucket))
           (bucket-add bucket route))
          ((eq bucket (first (node-buckets node)))
           (split-bucket node)
           (add-route node route)))))

(defun update-route (node id agent)
  (find-if (lambda (route)
             (when (= (route-id route) id)
               (setf (route-agent route) agent
                     (route-ctime route) (get-internal-real-time))))
           (bucket-routes (find-bucket id node))))

(defun find-routes (key node)
  (reduce (lambda (closest route &aux (previous (first closest)))
            (if (or (null closest)
                    (< (distance (route-id route) key)
                       (distance (route-id previous) key)))
                (list route) closest))
          (bucket-routes (find-bucket key node))
          :initial-value nil))

(defun refresh-bucket-p (bucket)
  (or (plusp (bucket-free bucket))
      (find-if 'route-stale-p (bucket-routes bucket))))

(defstruct ring
  (sequence 0) buffer)

(defun ring (size)
  (make-ring :buffer (make-array size :initial-element nil)))

(defun ring-position (ring sequence)
  (mod sequence (length (ring-buffer ring))))

(defun ring-push (ring value)
  (prog1 #1=(ring-sequence ring)
    (setf (aref (ring-buffer ring) (ring-position ring #1#)) value)
    (incf #1#)))

(defun exceeds-ring-p (ring sequence)
  (> (- (ring-sequence ring) sequence) (length (ring-buffer ring))))

(defun ring-get (ring sequence)
  (unless (exceeds-ring-p ring sequence)
    (aref (ring-buffer ring) (ring-position ring sequence))))

(defun ring-delete (ring sequence)
  (unless (exceeds-ring-p ring sequence)
    (setf (aref (ring-buffer ring) (ring-position ring sequence)) nil)))

(defparameter *node* nil)

(defstruct message
  (id (and *node* (node-id *node*))) (agent (agent)) sequence)

(defstruct (request (:include message))
  (forward-p t) trace-p)

(defstruct (reply (:include message)))

(defun reply-bind (request &optional callback-function)
  (setf (message-sequence request)
        (ring-push (node-requests *node*) callback-function))
  request)

(defun finalize-request (reply)
  (ring-delete (node-requests *node*) (message-sequence reply)))

(defun replicate-request (request &key (id (node-id *node*)) (forward-p t))
  (let ((replica (copy-structure request)))
    (setf (message-id replica) id
          (message-agent replica) (agent)
          (message-sequence replica) nil
          (request-forward-p replica) forward-p)
    replica))

(defun accept (reply)
  (let ((callback (when #1=(message-sequence reply)
                    (ring-get (node-requests *node*) #1#))))
    (when callback
      (funcall callback reply))))

(defun forward (request routes)
  (dolist (route routes routes)
    (when (request-trace-p request)
      (write-log `(:forward ,request ,route)))
    (send request (route-agent route))))

(defun respond (reply request)
  (setf (message-sequence reply) (message-sequence request))
  (when (request-trace-p request)
    (write-log `(:respond ,request ,reply)))
  (send reply (message-agent request)))

(defun intern-route (message)
  (with-slots (id agent) message
    (or (update-route *node* id agent)
        (add-route *node* (route id agent)))))

(defstruct (discover-request (:include request)) key)
(defstruct (discover-reply (:include reply)))

(defstruct (get-request (:include request)) key)
(defstruct (get-reply (:include reply)) value)

(defstruct (put-request (:include request)) key value)
(defstruct (put-reply (:include reply)))

(defstruct (delete-request (:include request)) key)
(defstruct (delete-reply (:include reply)))

(defmethod values-get ((values hash-table) key)
  (gethash key values))

(defmethod values-put ((values hash-table) key value)
  (setf (gethash key values) value))

(defmethod values-delete ((values hash-table) key)
  (remhash key values))

(defun routes (from to)
  (remove-if (lambda (route)
               (or (<= (distance (node-id *node*) to)
                       (distance (route-id route) to))
                   (eql (route-id route) from)))
             (find-routes to *node*)))

(defun neighbors ()
  (let ((n *replication*))
    (loop for bucket in (node-buckets *node*) while (plusp n) append
         (loop for route in (bucket-routes bucket) while (plusp n)
            do (decf n) collect route))))

(defun discover (key &optional announce-p)
  (forward (make-discover-request :id (and announce-p (node-id *node*))
                                  :key key)
           (find-routes key *node*)))

(defmethod handle ((request discover-request))
  (with-slots (id key) request
    (respond (make-discover-reply) request)
    (forward request (routes id key))))

(defmethod handle ((request get-request))
  (with-slots (id key forward-p) request
    (unless (forward request (routes id key))
      (multiple-value-bind (value exists-p)
          (values-get #1=(node-values *node*) key)
        (cond (exists-p
               (respond #2=(make-get-reply :value value) request))
              (forward-p
               (forward (reply-bind (replicate-request request :forward-p nil)
                                    (lambda (reply)
                                      (with-slots (value) reply
                                        (values-put #1# key value)
                                        (respond #2# request))
                                      (finalize-request reply)))
                        (neighbors))))))))

(defmethod handle ((request put-request))
  (with-slots (id key value forward-p) request
    (unless (forward request (routes id key))
      (values-put (node-values *node*) key value)
      (respond (make-put-reply) request)
      (when forward-p
        (forward (replicate-request request :forward-p nil) (neighbors))))))

(defmethod handle ((request delete-request))
  (with-slots (id key forward-p) request
    (unless (forward request (routes id key))
      (values-delete (node-values *node*) key)
      (respond (make-delete-reply) request)
      (when forward-p
        (forward (replicate-request request :forward-p nil) (neighbors))))))

(defun gen-id (&optional (start 0) (end (expt 2 *key-size*)))
  (+ start (random (- end start))))

(defun random-bucket-id (bucket)
  (gen-id (expt 2 (1- (bucket-bound bucket)))
          (expt 2 (bucket-bound bucket))))

(defun refresh-routes (&optional announce-p)
  (loop for bucket in (node-buckets *node*) when (refresh-bucket-p bucket)
     do (discover (random-bucket-id bucket) announce-p)))

(defun initialize-node (routes &optional announce-p)
  (dolist (route routes)
    (add-route *node* route))
  (discover (node-id *node*) announce-p))

(defun refresh-interval ()
  (/ *timeout* internal-time-units-per-second 3))

(defun receive-message ()
  (handler-case (receive :timeout (refresh-interval))
    (:no-error (message)
      (when (message-id message)
        (intern-route message))
      message)
    (timeout (timeout)
      (declare (ignore timeout)))))

(defun node (&key (id (gen-id)) routes (values (make-hash-table)))
  (let ((*node* (make-node :id id :values values))
        (*random-state* (make-random-state t)))
    (initialize-node routes :announce)
    (loop for message = (receive-message) do
         (etypecase message
           (request (handle message))
           (reply   (accept message))
           (null    (refresh-routes :announce))))))

(defun proxy (request)
  (forward (reply-bind (replicate-request request :id nil)
                       (lambda (reply)
                         (finalize-request reply)
                         (respond reply request)))
           (find-routes (slot-value request 'key) *node*)))

(defun client (&key routes)
  (let ((*node* (make-node :id (gen-id)))
        (*random-state* (make-random-state t)))
    (initialize-node routes)
    (loop for message = (receive-message) do
         (etypecase message
           (request (proxy message))
           (reply   (accept message))
           (null    (refresh-routes))))))
