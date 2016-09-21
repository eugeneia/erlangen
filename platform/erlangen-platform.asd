;;;; System definition for ERLANGEN-PLATFORM.

(defsystem erlangen-platform
  :description
  "Reusable component framework for Erlangen."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "Not licensed"
  :components ((:file "log")
               (:file "supervisor")
               (:file "server"))
  :depends-on ("erlangen" "optima"))
