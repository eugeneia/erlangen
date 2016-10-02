(defpackage erlangen-platform.server
  (:use :cl :erlangen :trivia)
  (:export :make-server
           :cast
           :call
           :no-reply))

(in-package :erlangen-platform.server)

(defun handle-request (request serve state)
  (match request
    ((cons from query) (multiple-value-bind (new-state reply)
                           (funcall serve query from state)
                         (unless (eq reply 'no-reply)
                           (ignore-errors (send reply from)))
                         new-state))
    (otherwise state)))

(defun make-server (&key init serve)
  "DOCUMENT ME! I AM PUBLIC."
  (lambda ()
    (loop for state = (funcall init)
       then (handle-request (receive) serve state))))

(defun cast (server query)
  "DOCUMENT ME! I AM PUBLIC."
  (send (cons (agent) query) server))

(defun call (server query)
  "DOCUMENT ME! I AM PUBLIC."
  (cast server query)
  (receive))
