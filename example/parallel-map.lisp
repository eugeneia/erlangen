(defpackage erlangen.examples
  (:use :cl :erlangen :optima)
  (:export :parallel-map))

(in-package :erlangen.examples)

(defun dice (n-chunks length)
  (let ((n-chunks (min n-chunks length)))
    (multiple-value-bind (chunk-size remainder) (floor length n-chunks)
      (loop for end from chunk-size to (* chunk-size n-chunks) by chunk-size
            for start = (- end chunk-size)
         if (< (+ end remainder) length) collect
           (cons start end)
         else collect
           (cons start (+ end remainder))))))

(defun map-worker (function result-type idx)
  (lambda ()
    (destructuring-bind (in start . end) (receive)
      (let ((out (make-array (- end start) :element-type result-type)))
        (loop for i from start below end do
             (setf (aref out (- i start)) (funcall function (aref in i))))
        (cons idx out)))))

(defun parallel-map (function vector &key (level 2) (result-type t))
  (let* ((chunks (dice level (length vector)))
         (workers (loop for idx from 1 to (length chunks) collect
                       (spawn (map-worker function result-type idx)
                              :attach :monitor))))
    (loop for chunk in chunks
          for worker in workers
       do (send (cons vector chunk) worker))
    (let ((results (loop for chunk in chunks collect
                        (ematch (receive)
                          ((list _ :ok result)
                           result)
                          ((list* agent :exit reason)
                           (exit (cons agent reason)))))))
      (apply 'concatenate 'vector
             (mapcar 'cdr (sort results '< :key 'car))))))

;; (let ((pmap (spawn (parallel-map '+ ...) :attach :link)))
;;   (destructuring-bind (from &rest result) (receive)
;;     (assert (equal from pmap))
;;     (print result)))
