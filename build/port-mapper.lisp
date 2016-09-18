;;;; port-mapper: Standalone port mapper daemon.

(require :asdf)

(asdf:load-system :erlangen)

(gc)

(save-application
 "bin/erlangen-port-mapper"
 :toplevel-function
 (lambda ()
   (erlangen:root ()
     (erlangen.distribution.protocol.port-mapper:port-mapper)))
 :error-handler :quit
 :purify t
 :prepend-kernel t)

(quit 0)
