(defpackage erlangen-platform.socket-server
  (:use :cl :ccl :erlangen :erlangen-platform.supervisor)
  (:export :socket-server
           :socket-responder))

(in-package :erlangen-platform.socket-server)

(defun make-listener-socket (host port options)
  (apply 'make-socket
         :connect :passive
         :local-host host
         :local-port port
         :reuse-address t
         options))

(defun responder-id (n)
  (intern (format nil "SOCKET-RESPONDER-~a" n) :keyword))

(defun responder-call (responder socket options)
  `(,responder ,socket ,@options))

(defun socket-server (&key (host "localhost") port
                           responder extra-arguments (n-responders 1)
                           socket-arguments
                           supervisor-arguments)
  (let ((socket (make-listener-socket host port socket-arguments)))
    (unwind-protect
         (apply 'supervisor
                (loop for i from 1 to n-responders collect
                     `(,(responder-id i)
                       ,(responder-call responder socket extra-arguments)))
                supervisor-arguments)
      (close socket :abort t))))

(defun socket-responder (socket responder-fn)
  (loop do (select ((accept-connection socket :wait nil) (connection)
                    (unwind-protect (funcall responder-fn connection)
                      (close connection))))))
