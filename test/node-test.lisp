;;;; Basic test for node protocol.

(defpackage erlangen.distribution.protocol.node-test
  (:use :cl
        :erlangen
        :erlangen.distribution.protocol.node
        :erlangen.distribution.protocol.port-mapper
        :erlangen.distribution.id)
  (:export :run-tests))

(in-package :erlangen.distribution.protocol.node-test)

(let (messages)
  (defun test-agent ()
    (setf messages nil)
    (loop do (push (receive) messages)))
  (defun test-messages ()
    messages))

(defun run-tests ()
  (let (port-mapper node-server-agent register-agent id)
    (unwind-protect
         (progn
           (setq port-mapper (spawn `(port-mapper)))
           (sleep 1)
           (multiple-value-bind (node-server port)
               (make-node-server :host (host-name))
             (setq node-server-agent (spawn node-server))
             (setq register-agent
                   (spawn `(register-node ,(node-name) ,port))))
           (sleep 1)

           ;; Test REMOTE-SPAWN
           (setq id (remote-spawn (host-name) (node-name) '(test-agent)
                                  "bar" :link 1))
           (assert (find-agent id) ()
                   "REMOTE-SPAWN failed.")
           (handler-case (remote-spawn
                          (host-name) (node-name) '(test-agent)
                          "nil" :invalid 1)
             (error (error)
               (declare (ignore error)))
             (:no-error ()
               (error "REMOTE-SPAWN succeeded with invalid mode.")))
           (handler-case (remote-spawn
                          (host-name) (node-name) '(invalid)
                          "nil" nil 1)
             (error (error)
               (declare (ignore error)))
             (:no-error ()
               (error "REMOTE-SPAWN succeeded with invalid call.")))

           ;; Test REMOTE-SEND
           (remote-send "hello" id)
           (assert (equal '("hello") (test-messages)) ()
                   "REMOTE-SEND failed.")
           (handler-case
               (let ((id (remote-spawn (host-name) (node-name) '(sleep 2)
                                       "nil" nil 1)))
                 (remote-send "hello" id)
                 (remote-send "hello2" id))
             (error (error)
               (declare (ignore error)))
             (:no-error ()
               (error "REMOTE-SEND succeeded even though message was not delivered.")))
           (handler-case (remote-send "hello" (agent-id :invalid))
             (error (error)
               (declare (ignore error)))
             (:no-error ()
               (error "REMOTE-SEND succeeded with invalid id.")))

           ;; Test REMOTE-LINK
           (remote-link id "foo" :link)
           (assert (equal (erlangen.agent::agent-links
                           (find-agent id))
                          '("foo" "bar")))
           (remote-link id "foo" :monitor)
           (assert (equal (erlangen.agent::agent-monitors
                           (find-agent id))
                          '("foo")))
           (handler-case (remote-link id "foo" :invalid)
             (error (error)
               (declare (ignore error)))
             (:no-error ()
               (error "REMOTE-LINK succeeded with invalid mode.")))
           (handler-case (remote-link (agent-id :invalid) "foo" :link)
             (error (error)
               (declare (ignore error)))
             (:no-error ()
               (error "REMOTE-LINK succeeded with invalid id.")))

           ;; Test REMOTE-UNLINK
           (remote-unlink id "foo")
           (assert (equal (erlangen.agent::agent-links
                           (find-agent id))
                          '("bar")))
           (assert (equal (erlangen.agent::agent-monitors
                           (find-agent id))
                          '()))
           (handler-case (remote-unlink (agent-id :invalid) "foo")
             (error (error)
               (declare (ignore error)))
             (:no-error ()
               (error "REMOTE-UNLINK succeeded with invalid id."))))
      (ignore-errors (exit :kill (find-agent id)))
      (ignore-errors (exit :kill register-agent))
      (ignore-errors (exit :kill node-server-agent))
      (ignore-errors (exit :kill port-mapper)))))
