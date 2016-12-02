;;;; Generic algorithms

(in-package :erlangen.algorithms)

(defun repeat-pace (function &key (delta 0.01) (maxsleep 0.1))
  "Repeatedly call FUNCTION, but pace interval "
  (loop with sleep = 0 for progress = (funcall function)
     if progress do
       (setf sleep 0)
     else do
       (setf sleep (max (+ sleep delta) maxsleep))
       (sleep sleep)))

(let ((float-time-units (float internal-time-units-per-second)))
  (defun now ()
    (/ (get-internal-real-time) float-time-units)))

(defun repeat-rate (function &key (hz 1))
  (let ((interval (/ 1 hz))
        (start (now)))
    (loop do
         (funcall function)
         (let* ((next (+ start interval))
                (now (now))
                (sleep (- next now)))
           (when (> sleep 0)
             (sleep sleep))
           (setf start (max now next))))))
