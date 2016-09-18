;;;; Basic test for port-mapper.

(defpackage erlangen.distribution.protocol.port-mapper-test
  (:use :cl
        :erlangen
        :erlangen.distribution.protocol.port-mapper)
  (:export :run-tests))

(in-package :erlangen.distribution.protocol.port-mapper-test)

(defun run-tests (&key (registry-port 10001)
                       (directory-port 20002))
  (let (port-mapper node1)
    (unwind-protect
         (progn
           (setq port-mapper
                 (spawn `(port-mapper
                          :registry-port ,registry-port
                          :directory-port ,directory-port)))
           (sleep 2)
           (setq node1
                 (spawn `(register-node "node1" 12345
                                        :registry-port ,registry-port)))
           (sleep 2)
           (assert
            (eql (query-node-port "localhost" "node1"
                                  :directory-port directory-port)
                 12345))
           (assert
            (equal (query-nodes "localhost"
                                :directory-port directory-port)
                   '(("node1" . 12345))))
           (handler-case
               (register-node "node1" 12345
                              :registry-port registry-port)
             (simple-error (error) (declare (ignore error)))
             (:no-error ()
               ;; This branch will never be taken, because REGISTER-NODE
               ;; wont exit on success.
               (error "Was able to register name `node1' twice.")))
           (exit :kill node1)
           (sleep 2)
           (handler-case (query-node-port "localhost" "node1"
                                          :directory-port directory-port)
             (error (error) error)
             (:no-error (value)
               (declare (ignore value))
               (error "Failed to unregister `node1'.")))
           (assert
            (equal (query-nodes "localhost"
                                :directory-port directory-port)
                   nil))
           (exit :kill port-mapper))
      (ignore-errors (exit :kill port-mapper))
      (ignore-errors (exit :kill node1)))))
