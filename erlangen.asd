;;;; System definition for ERLANGEN.

(defsystem erlangen
  :description
  "Distributed asychronous message passing system for Common Lisp."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "Not licensed"
  :components ((:file "mailbox")
               (:file "agent" :depends-on ("mailbox"))
               (:file "registry" :depends-on ("agent"))
               (:file "erlangen" :depends-on ("agent" "registry"))
                ;; Tests
               (:file "test/agent-test" :depends-on ("agent"))))
