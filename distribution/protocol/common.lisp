;;;; Common protocol subset.

(in-package :erlangen.distribution.protocol.common)

(defparameter *protocol-version* #x00
  "Protocol version implemented by ERLANGEN.DISTRIBUTION.PROTOCOL.*; to
be bumped in the event of changes to the protocol.")

(defparameter *i/o-timeout* (* 10 1000) ; There is a bug in CCL (1.10?)
                                        ; where TCP stream timeouts are
                                        ; interpreted as millisecond
                                        ; values.
  "Default input/output timeout for TCP stream used throughout the
protocol implementation.")

(defun make-socket* (&rest other-keys)
  "Returns a socket connected to PORT on HOST."
  (apply 'make-socket
         :input-timeout *i/o-timeout*
         :output-timeout *i/o-timeout*
         other-keys))

(defmacro with-socket ((var socket) &body body
                       &aux (done-sym (gensym "done")))
  "Execute BODY with VAR bound to SOCKET. SOCKET is closed on exit."
  `(let ((,var ,socket) ,done-sym)
     (unwind-protect (multiple-value-prog1 (progn ,@body)
                       (setq ,done-sym t))
       (close ,var :abort (not ,done-sym)))))

(defmacro do-request ((socket-var) request &optional expected-reply)
  "REQUEST has the form (REQUEST-FN &rest ARGS) where REQUEST-FN is the
request writer to be called and ARGS its arguments. EXPECTED-REPLY has
the form (TYPE REPLY-FN) where TYPE is an unsigned integer and REPLY-FN
is the reply reader to be called. Sends REQUEST over socket bound to
SOCKET-VAR. If EXPECTED-REPLY is supplied reads reply as specified,
otherwise expects ACK-REPLY. If reply is an ERROR-REPLY the appropriate
error is signaled."
  (check-type socket-var symbol)
  `(progn
     ,`(,@request ,socket-var)
     (force-output ,socket-var)
     (multiple-value-bind (type reply) (read-message ,socket-var)
       (ecase type
         (#x01 (error (read-error-reply reply)))
         ,(if expected-reply
              (destructuring-bind (reply-id reply-reader) expected-reply
                `(,reply-id (,reply-reader reply)))
              '(#x02 (values)))))))

;; Basic protocol messages. HELLO-REPLY is sent to connecting clients (as
;; a response to a protocol setup request implied by connection
;; establishment). It contains the protocol version offered by the
;; server. If the client does not implement the protocol version
;; specified by the server's HELLO-REPLY it must close the connection.
;;
;; ERROR-REPLY is a generic reply sent to clients when a request can not
;; be successfully served for any reason. It contains a string that
;; further describes the failure reason.
;;
;; ACK-REPLY is a generic reply sent to clients when a request was
;; successfully processed by the server. It contains no additional
;; information. It is generally used to acknowledge requests that do not
;; yield any datum.
(define-message #x00 hello-reply (version integer))
(define-message #x01 error-reply (description string))
(define-message #x02 ack-reply)

(defun send-hello (connection)
  "Sends HELLO-REPLY on CONNECTION."
  (write-hello-reply *protocol-version* connection))

(defun assert-protocol-version (socket)
  "Receives HELLO-REPLY on SOCKET. If the specified protocol version does
not match this implementation an error is signaled."
  (let ((remote-version (read-message* #x00 'read-hello-reply socket)))
    (unless (= remote-version *protocol-version*)
      (error "Protocol version mismatch (local: ~a, remote ~a)."
             *protocol-version* remote-version)))
  (values))
