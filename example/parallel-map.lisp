(defpackage erlangen.examples
  (:use :cl :erlangen :trivia)
  (:export :parallel-map))

(in-package :erlangen.examples)

;; (spawn '(parallel-map 1+ #(2 4 6 8 10 12 14) :level 3) :attach :monitor)
;; (receive)

(defun worker ()
  (ematch (receive)
    ((and (type function) function)
     (funcall function))))

(defun map-chunk (function vector start end result-type)
  (lambda ()
    (let ((results (make-array (- end start) :element-type result-type)))
      (loop for i from start below end do
           (setf (aref results (- i start))
                 (funcall function (aref vector i))))
      (values start end results))))

(defun parallel-map (function vector &key (level 2) (result-type t))
  (let* ((length (length vector))
         (n-chunks (min level length))
         (chunk-size (floor length n-chunks))
         (workers (loop for i from 1 to n-chunks collect
                       (spawn 'worker :attach :monitor))))

    (loop for worker in workers
          for chunk from 1
          for start from 0 by chunk-size
          for end = (if (< chunk n-chunks)
                        (+ start chunk-size)
                        length)
       do (send (map-chunk function vector start end result-type) worker))
    (loop with results = (make-array length :element-type result-type)
          for worker in workers do
         (ematch (receive)
           ((list (type agent) :ok start end chunk-result)
            (replace results chunk-result :start1 start :end1 end))
           ((list (type agent) :exit reason)
            (exit reason)))
       finally (return results))))
