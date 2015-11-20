;;;; Erlangen port mapper protocol implementation. The port mapper
;;;; protocol consists of two parts: The directory and the
;;;; registry. Remote nodes can query the host's directory to find a
;;;; node's listening port on that host. Local nodes can register a port
;;;; with the registry so that they can be found by remote nodes.

(in-package :erlangen.distribution.protocol.port-mapper)

;; Registry protocol messages. REGISTER-REQUEST is sent to the registry
;; service in order to register a port. It contains the client node's
;; name and the port it wishes to register. The registry service must
;; either acknowledge and process the request by sending an ACK-REPLY or
;; decline to process the request by sending an ERROR-REPLY.
(define-message #x10 register-request (node-name string) (port integer))

;; Directory protocol messages. PORT-REQUEST is sent to the directory
;; service in order to obtain a given node's listening port. It contains
;; the name of the node of interest. The directory service must either
;; satisfy the query by sending with a PORT-REPLY or indicate failure to
;; do so by sending an ERROR-REPLY (e.g. if the node of interest is not
;; registered).
;;
;; PORT-REPLY is sent by the directory service to satisfy a query
;; initiated by a PORT-REQUEST. It contains the port of the node of
;; interest.
;;
;; NODES-REQUEST is sent to the directory service in order to obtain a
;; mapping the names of the registered nodes to their respective
;; listening ports. The directory service must either satisfy the query
;; by sending with a NODES-REPLY or indicate failure to do so by sending
;; an ERROR-REPLY.
;;
;; NODES-REPLY is sent by the directory service to satisfy a query
;; initiated by a NODES-REQUEST. It contains an association list mapping
;; the names of the registered nodes to their respective listening ports.
(define-message #x20 port-request (node-name string))
(define-message #x21 port-reply (port integer))
(define-message #x22 nodes-request)
(define-message #x23 nodes-reply (nodes value))

(defstruct server-context
  "Context of a port mapper."
  (nodes (make-hash-table :test 'equal))
  (lock (make-read-write-lock)))

(defun keep-open (connection)
  "Blocks until CONNECTION is closed by the peer."
  (setf (stream-input-timeout connection) nil)
  (setf (stream-output-timeout connection) nil)
  (select ((eql (read-char-no-hang connection nil :eof) :eof)
           (closed-p))))

(defun acquire-port (context name port)
  "Attempts to acquire record for NAME and PORT in CONTEXT. Returns PORT
on success. If a record for NAME already exists, ACQUIRE-PORT returns NIL
to indicate failure."
  (with-slots (lock nodes) context
    (with-write-lock (lock)
      (unless #1=(gethash name nodes)
        (setf #1# port)))))

(defun release-port (context name)
  "Releases the record associated to NAME in CONTEXT."
  (with-slots (lock nodes) context
    (with-write-lock (lock)
      (remhash name nodes))))

(defun serve-registry (context connection)
  "Serves a registry service CONNECTION in CONTEXT."
  (send-hello connection)
  (force-output connection)
  (multiple-value-bind (name port)
      (read-message* #x10 'read-register-request connection)
    (case (acquire-port context name port)
      ((nil)
       (write-error-reply "Node name already in use." connection)
       (force-output connection))
      (otherwise
       (write-ack-reply connection)
       (force-output connection)
       (unwind-protect (keep-open connection)
         (release-port context name))))))

(defun make-register-request-handler (context connection)
  "Returns handler for registry service CONNECTION in CONTEXT."
  (lambda ()
    (unwind-protect (serve-registry context connection)
      (close connection :abort t))))

(defun make-port-registry (context host port)
  "Returns listener for registry service on PORT of HOST in CONTEXT."
  (lambda ()
    (with-listen (socket host port :keepalive t)
      (loop do (select
                ((accept-connection socket :wait nil) (connection)
                  (spawn (make-register-request-handler
                          context connection)
                         :attach :monitor)))))))

(defun node-port (context name)
  "Returns the port associated to NAME in CONTEXT."
  (with-slots (lock nodes) context
    (with-read-lock (lock)
      (gethash name nodes))))

(defun handle-port-request (context connection request)
  "Handles port-request REQUEST on CONNECTION in CONTEXT."
  (let ((port (multiple-value-bind (name) (read-port-request request)
                (node-port context name))))
      (if port
          (write-port-reply port connection)
          (write-error-reply "No such node." connection))))

(defun nodes (context)
  "Returns an alist mapping node names to ports in CONTEXT."
  (with-slots (lock nodes) context
    (with-read-lock (lock)
      (loop for name being the hash-keys of nodes
         for port being the hash-values of nodes
         collect (cons name port)))))

(defun handle-nodes-request (context connection request)
  "Handles nodes-request REQUEST on CONNECTION in CONTEXT."
  (declare (ignore request))
  (write-nodes-reply (nodes context) connection))

(defun serve-directory (context connection)
  "Serves a directory service CONNECTION in CONTEXT."
  (send-hello connection)
  (force-output connection)
  (multiple-value-bind (type request) (read-message connection)
    (ecase type
      (#x20 (handle-port-request context connection request))
      (#x22 (handle-nodes-request context connection request)))))

(defun make-port-directory (context host port)
  "Returns listener for directory service on PORT of HOST in CONTEXT."
  (lambda ()
    (with-listen (socket host port)
      (loop do (select
                ((accept-connection socket :wait nil) (connection)
                  (ignore-errors
                    (unwind-protect (serve-directory context connection)
                      (close connection)))))))))

(defun port-mapper (&key (registry-host "localhost")
                         (registry-port 10001)
                         (directory-host "0.0.0.0")
                         (directory-port 20002))
  "Starts a port mapper instance. Registry service listens on
REGISTRY-PORT (defaults to 10001) of REGISTRY-HOST (defaults to
\"localhost\"). Directory service listens on DIRECTORY-PORT (defaults to
20002) of DIRECTORY-HOST (defaults to \"0.0.0.0\")."
  (let ((context (make-server-context)))
    (spawn (make-port-registry context registry-host registry-port)
           :attach :link)
    (spawn (make-port-directory context directory-host directory-port)
           :attach :link))
  (receive))
  
(defun register-node (name port &key (registry-host "localhost")
                                     (registry-port 10001))
  "Register PORT for node by NAME on registry service listening on
REGISTRY-PORT (defaults to 10001) of REGISTRY-HOST (defaults to
\"localhost\")."
  (with-connect (socket registry-host registry-port)
    (assert-protocol-version socket)
    (write-register-request name port socket)
    (force-output socket)
    (multiple-value-bind (type reply) (read-message socket)
      (ecase type
        (#x01 (error (read-error-reply reply)))
        (#x02 (keep-open socket))))))

(defun query-node-port (host node-name &key (directory-port 20002))
  "Query directory service on DIRECTORY-PORT (defaults to 20002) of HOST
for port of node by NODE-NAME. Returns the port number on success or NIL
and a string describing the failure."
  (with-connect (socket host directory-port)
    (assert-protocol-version socket)
    (write-port-request node-name socket)
    (force-output socket)
    (multiple-value-bind (type reply) (read-message socket)
      (ecase type
        (#x01 (values nil (read-error-reply reply)))
        (#x21 (read-port-reply reply))))))

(defun query-nodes (host &key (directory-port 20002))
  "Query directory service on DIRECTORY-PORT (defaults to 20002) of HOST
for node-to-port mapping. Returns an alist on success or signals an error
on failure."
  (with-connect (socket host directory-port)
    (assert-protocol-version socket)
    (write-nodes-request socket)
    (force-output socket)
    (multiple-value-bind (type reply) (read-message socket)
      (ecase type
        (#x01 (error (read-error-reply reply)))
        (#x23 (read-nodes-reply reply))))))
