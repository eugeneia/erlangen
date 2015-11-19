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
               (:file "macros"
                      :depends-on ("algorithms"))
               (:file "distribution/call")
               (:file "distribution/protocol/buffers")
               (:file "distribution/protocol/common"
                      :depends-on ("distribution/protocol/buffers"))
               (:file "distribution/protocol/port-mapper"
                      :depends-on ("distribution/protocol/buffers"
                                   "distribution/protocol/common"
                                   "macros"))
               (:file "erlangen"
                      :depends-on ("agent"
                                   "registry"
                                   "conditions"
                                   "macros"
                                   "distribution/call"))
               ;; Tests
               (:file "test/agent-test"
                      :depends-on ("agent")))
  :depends-on ("jpl-queues"
               "bordeaux-threads"
               "trivial-utf-8"
               "fast-io"
               "cl-conspack"))
