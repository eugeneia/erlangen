;;;; Erlangen node protocol implementation. The node protocol defines a
;;;; scheme to issue SPAWN, SEND, LINK and UNLINK remotely. Any node can
;;;; act as a client connecting to another node on its listening port. At
;;;; the same time every node must listen for incoming requests on its
;;;; listening port. The listening port of a node is retrieved from a
;;;; port mapper instance.
;;;;
;;;; In the context of this protocol, “local” and “remote” describe the
;;;; client and server nodes respectively, but do not imply that these
;;;; must necessarily be on different hosts.

(in-package :erlangen.distribution.protocol.node)

;; SPAWN-REQUEST is sent by a node to issue a SPAWN. It contains a CALL
;; (see ERLANGEN.DISTRIBUTION.CALL), the parent agent identifier, the
;; attach mode (:LINK, :ATTACH or NIL) and the mailbox capacity of the
;; agent to create. The remote node must either spawn the agent
;; accordingly and acknowlege the request by sending a SPAWN-REPLY or
;; indicate failure to do so by sending an ERROR-REPLY.
;;
;; SPAWN-REPLY is sent by nodes to acknowledge a SPAWN-REQUEST. It
;; contains the agent identifier of the spawned agent.
(define-message #x30 spawn-request
  (call value) (parent string) (attach value) (mailbox-size integer))
(define-message #x31 spawn-reply (agent string))

;; SEND-REQUEST is sent by a node to issue a SEND. It contains the
;; message to be delivered and the target agent. The remote node must
;; either acknowledge the delivery of the message by sending an ACK-REPLY
;; or indicate failure to do so by sending an ERROR-REPLY.
(define-message #x32 send-request (message value) (agent string))

;; LINK-REQUEST is sent by a node to issue a LINK. It contains a remote
;; agent identifier and a local agent identifier as well as the desired
;; link mode (:LINK or :ATTACH). The remote node must either link the
;; specified agents accordingly and acknowlege the request by sending an
;; ACK-REPLY or indicate failure to do so by sending an ERROR-REPLY.
;;
;; UNLINK-REQUEST is sent by a node to issue an UNLINK. It contains a
;; remote agent identifier and a local agent identifier. The remote node
;; must either unlink the specified agents accordingly and acknowlege the
;; request by sending an ACK-REPLY or indicate failure to do so by
;; sending an ERROR-REPLY.
(define-message #x34 link-request
  (remote-agent string) (local-agent string) (mode value))
(define-message #x36 unlink-request
  (remote-agent string) (local-agent string))

(defun handle-spawn-request (connection request)
  "Handles REQUEST of type SPAWN-REQUEST on CONNECTION."
  (multiple-value-bind (call parent attach mailbox-size)
      (read-spawn-request request)
    (handler-case (spawn (make-function call)
                         :attach attach
                         :to parent
                         :mailbox-size mailbox-size)
      (type-error (error)
        (declare (ignore error))
        (write-error-reply "Invalid request." connection))
      (error (error)
        (declare (ignore error))
        (write-error-reply "Unable to spawn agent." connection))
      (:no-error (agent)
        (write-spawn-reply (agent-id agent) connection)))))

(defun handle-send-request (connection request)
  "Handles REQUEST of type SEND-REQUEST on CONNECTION."
  (multiple-value-bind (message agent-id) (read-send-request request)
    (let ((agent (find-agent agent-id)))
      (if agent
          (handler-case (send message agent)
            (error (error)
              (declare (ignore error))
              (write-error-reply
               "Unable to deliver message." connection))
            (:no-error () (write-ack-reply connection)))
          (write-error-reply "No such agent." connection)))))

(defun handle-link-request (connection request)
  "Handles REQUEST of type LINK-REQUEST on CONNECTION."
  (multiple-value-bind (local-agent remote-agent mode)
      (read-link-request request)
    (let ((agent (find-agent local-agent)))
      (if agent
          (handler-case (add-link agent mode remote-agent)
            (type-error (error)
              (declare (ignore error))
              (write-error-reply "Invalid request." connection))
            (error (error)
              (declare (ignore error))
              (write-error-reply "Unable to link." connection))
            (:no-error ()
              (write-ack-reply connection)))
          (write-error-reply "No such agent." connection)))))

(defun handle-unlink-request (connection request)
  "Handles REQUEST of type UNLINK-REQUEST on CONNECTION."
  (multiple-value-bind (local-agent remote-agent)
      (read-unlink-request request)
    (let ((agent (find-agent local-agent)))
      (if agent
          (handler-case (remove-link agent remote-agent)
            (error (error)
              (declare (ignore error))
              (write-error-reply "Unable to link." connection))
            (:no-error ()
              (write-ack-reply connection)))
          (write-error-reply "No such agent." connection)))))

(defun serve-requests (connection)
  "Serves requests on CONNECTION."
  (send-hello connection)
  (force-output connection)
  (loop do (select ((listen connection) (ready-p)
                    (multiple-value-bind (type request)
                        (read-message connection)
                      (funcall (ecase type
                                 (#x30 'handle-spawn-request)
                                 (#x32 'handle-send-request)
                                 (#x34 'handle-link-request)
                                 (#x36 'handle-unlink-request))
                                 connection request)
                      (force-output connection))))))

(defun make-request-handler (connection)
  "Returns a handler for node protocol CONNECTION."
  (lambda ()
    (unwind-protect (serve-requests connection)
      (close connection))))

(defun make-node-server (&key (host "0.0.0.0"))
  "Returns listener for node protocol on HOST and the listening port."
  (let ((socket (make-socket* :connect :passive
                              :local-host host
                              :keepalive t)))
    (values (lambda ()
              (with-socket (socket socket)
                (loop do
                     (select
                      ((accept-connection socket :wait nil) (connection)
                       (spawn (make-request-handler connection)
                              :attach :monitor))))))
            (local-port socket))))

;; The local node opens and reuses one connection per remote node.
(defvar *remote-connections*/lock (make-lock))
(defvar *remote-connections* (make-hash-table :test 'equal)
  "Holds established connections to remote nodes.")

(defun make-connection (nid)
  "Returns a connection to remote node by NID of the form
(HOST-NAME . NODE-NAME). `Connection' is a cons containing a socket
connected to the remote node and the connection lock."
  (destructuring-bind (host . node) nid
    (cons (make-socket* :remote-host host
                        :remote-port (query-node-port host node)
                        :keepalive t)
          (make-lock))))

(defun close-connection (connection)
  "Closes CONNECTION."
  (close (car connection) :abort t)
  (setf (car connection) nil))

(defun establish-connection (nid)
  "Establishes a new connection to remote node by NID. On failure the
connection is closed and an error is signaled."
  (let ((connection (setf (gethash nid *remote-connections*)
                          (make-connection nid))))
    (grab-lock (cdr connection))
    (handler-case (assert-protocol-version (car connection))
      (error (error)
        (close-connection connection)
        (error error))
      (:no-error ()
        connection))))

(defun get-established-connection (nid)
  "Returns the establed connection to remote node by NID if it exists."
  (let ((connection (gethash nid *remote-connections*)))
    (when connection
      (grab-lock (cdr connection))
      (if (not (null (car connection)))
          connection
          (release-lock (cdr connection))))))

(defun get-connection (nid)
  "Returns the existing established connection to node by NID if it
exists. Otherwise establishes and returns a new connection."
  (with-lock-grabbed (*remote-connections*/lock)
    (or (get-established-connection nid)
        (establish-connection nid))))

(defun clear-connections ()
  "Closes all remote connections."
  (with-lock-grabbed (*remote-connections*/lock)
    (maphash (lambda (nid connection)
               (with-lock-grabbed ((cdr connection))
                 (when (car connection)
                   (close-connection connection))
                 (remhash nid *remote-connections*)))
             *remote-connections*)))

(defmacro with-connection ((var host node) &body body
                           &aux (connection-sym (gensym "connection")))
  "Evaluate BODY with VAR bound to a socket of an established connection
to NODE of HOST."
  `(let* ((,connection-sym (get-connection (cons ,host ,node)))
          (,var (car ,connection-sym)))
     (unwind-protect (handler-case (progn ,@body)
                       (error (error)
                         (close-connection ,connection-sym)
                         (error error)))
       (release-lock (cdr ,connection-sym)))))

(defun remote-spawn (host node call parent attach mailbox-size)
  "Spawns agent on remote NODE of HOST using CALL, PARENT, ATTACH mode
and MAILBOX-SIZE."
  (check-type call call)
  (check-type attach (member :link :monitor nil))
  (check-type mailbox-size (integer 1))
  (with-connection (socket host node)
    (do-request (socket)
      (write-spawn-request call parent attach mailbox-size)
      (#x31 read-spawn-reply))))

(defun remote-send (message id)
  "Sends MESSAGE to remote agent by ID."
  (multiple-value-bind (host node) (decode-id id)
    (with-connection (socket host node)
      (do-request (socket)
        (write-send-request message id)))))

(defun remote-link (remote-id local-id mode)
  "Links remote agent by REMOTE-ID to agent by LOCAL-ID using MODE."
  (check-type mode (member :link :monitor))
  (multiple-value-bind (host node) (decode-id remote-id)
    (with-connection (socket host node)
      (do-request (socket)
        (write-link-request remote-id local-id mode)))))

(defun remote-unlink (remote-id local-id)
  "Unlinks remote agent by REMOTE-ID from agent by LOCAL-ID."
  (multiple-value-bind (host node) (decode-id remote-id)
  (with-connection (socket host node)
    (do-request (socket)
      (write-unlink-request remote-id local-id)))))
