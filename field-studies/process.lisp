;;;; Process management.

#-(or ccl)
(error "This implementation is unsupported.")

#-openmcl-native-threads
(error "Clozure CL without :OPENMCL-NATIVE-THREADS is unsupported.")

(defpackage nodes.process
  (:use :cl)
  (:export :process-pool-size
           :use-process
           :reset-process))

(in-package :nodes.processes)

(defparameter *initial-pool-size* 8)

(defun make-process* ()
  (ccl:make-process "nodes"))

(defstruct process-pool
  (lock     (ccl:make-read-write-lock)
            :type ccl:read-write-lock)
  (active   nil
            :type list)
  (inactive (loop for n from 1 to *initial-pool-size*
               collect (make-process*))
            :type list)
  (zombies  nil
            :type list))

(defvar *process-pool* (make-process-pool))

(defun discard-process (process)
  (ccl:process-preset process (lambda ()))
  (handler-case (ccl:process-enable process 1)
    (error (e) (declare (ignore e))
           (push process (process-pool-zombies *process-pool*))))
  (ccl:process-kill process)
  (ccl:join-process process)
  (values))

(defun process-pool-size ()
  (ccl:with-read-lock ((process-pool-lock #1=*process-pool*))
    (+ (length (process-pool-active #1#))
       (length (process-pool-inactive #1#)))))

(defun set-process-pool-size (n)
  (let* ((size (process-pool-size))
         (diff (- n size)))
    (ccl:with-write-lock ((process-pool-lock #1=*process-pool*))
      (cond ((< diff 0)
             ;; Destroy up to N inactive processes.
             (setf (process-pool-inactive #1#)
                   (loop for i = diff then (1+ i)
                         for p in (process-pool-inactive #1#)
                      if (< i 0) do (discard-process p)
                      else collect p)))
            ((> diff 0)
             ;; Create N new inactive threads.
             (loop for i from 1 to diff do
                  (push (make-process*)
                        (process-pool-inactive #1#)))))))
  (values))

(defsetf process-pool-size set-process-pool-size)

(defun use-process (function)
  (ccl:with-write-lock ((process-pool-lock #1=*process-pool*))
    (let ((process (or (pop (process-pool-inactive #1#))
                       (make-process*))))
      (ccl:process-preset process function)
      (ccl:process-enable process)
      (push process (process-pool-active #1#))
      process)))

(defun reset-process (process)
  (ccl:with-write-lock ((process-pool-lock #1=*process-pool*))
    (ccl:process-reset process)
    (setf #2=(process-pool-active #1#) (remove process #2#))
    (push process (process-pool-inactive #1#))))
