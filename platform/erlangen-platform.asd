;;;; System definition for ERLANGEN-PLATFORM.

(defsystem erlangen-platform
  :description
  "Reusable component framework for Erlangen."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU Affero General Public License"
  :components ((:file "supervisor")
               (:file "logger")
               (:file "server")
               (:file "timer"))
  :depends-on ("erlangen" "trivia"))
