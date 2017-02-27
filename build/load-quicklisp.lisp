(let* ((default (merge-pathnames "quicklisp/" (user-homedir-pathname)))
       (quicklisp (probe-file (or (getenv "QUICKLISP") default))))
  (unless quicklisp
    (format *error-output*
     "Could not find Quicklisp, install Quicklisp to ~a
or set QUICKLISP to the path of your Quicklisp installation."
     default)
    (quit 1))
  (load (merge-pathnames "setup.lisp" quicklisp)))
