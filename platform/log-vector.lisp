;;;; Vector logs.

(defpackage erlangen-platform.log-vector
  (:use :cl :ccl :erlangen)
  (:export :make-log-vector
           :make-to-vector
           :pop-log-vector))

(in-package :erlangen-platform.log-vector)

(defstruct (log-vector (:constructor make-log-vector ()))
  (vector (make-array 0 :element-type 'list :adjustable t :fill-pointer t))
  (lock (make-lock "erlangen-platform.log-vector::log-vector")))

(defun to-vector (message log-vector)
  (with-slots (vector lock) log-vector
    (with-lock-grabbed (lock)
      (vector-push-extend message vector))))

(defun make-to-vector (log-vector)
  (lambda (message)
    (to-vector message log-vector)))

(defun pop-log-vector (log-vector)
  (with-slots (vector lock) log-vector
    (with-lock-grabbed (lock)
      (loop for i from 0 below (length vector)
         collect (aref vector i)
         do (setf (aref vector i) nil)
         finally (setf (fill-pointer vector) 0)))))
