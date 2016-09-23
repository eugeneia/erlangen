;;;; kernel: Erlangen kernel executable for standalone use or use as an
;;;; “inferior Lisp.”

(require :asdf)

(asdf:load-system :erlangen)

(push (lambda ()
        (use-package :erlangen :cl-user))
      *lisp-startup-functions*)

(gc)

(save-application "bin/erlangen-kernel" :prepend-kernel t)
