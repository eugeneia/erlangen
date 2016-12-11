;;;; kernel: Erlangen kernel executable for standalone use or use as an
;;;; “inferior Lisp.”

(require :asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-system :erlangen)

(push (lambda ()
        (use-package '(:erlangen :erlangen.management) :cl-user))
      *lisp-startup-functions*)

(gc)

(save-application "bin/erlangen-kernel" :prepend-kernel t)
