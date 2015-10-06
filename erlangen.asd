;;;; System definition for ERLANGEN.

(defsystem erlangen
  :description
  "Distributed asychronous message passing system for Common Lisp."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "Not licensed"
  :components ((:file "conditions")
               (:file "mailbox")
               (:file "algorithms")
               (:file "agent"
                      :depends-on ("mailbox" "algorithms" "conditions"))
               (:file "registry"
                      :depends-on ("agent"))
               (:file "erlangen"
                      :depends-on ("agent" "registry" "conditions"))
               ;; Tests
               (:file "test/agent-test"
                      :depends-on ("agent")))
  :depends-on ("jpl-queues"
               "bordeaux-threads"))
