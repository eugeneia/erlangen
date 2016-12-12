;;;; System definition for ERLANGEN-PLATFORM.

(defsystem erlangen-platform
  :description
  "Reusable component framework for Erlangen."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "AGPL-3.0"
  :components ((:file "supervisor")
               (:file "logger")
               (:file "server")
               (:file "timer"))
  :depends-on ("erlangen" "trivia"))
