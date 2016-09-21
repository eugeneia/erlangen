;;;; Generic supervisor agent.
;;;; TODO: Accept MAILBOX-SIZE in childspec.
;;;; TODO: Accept more complete childspec.
;;;; TODO: Add more strategies.

(defpackage erlangen-platform.supervisor
  (:use :cl :erlangen :optima)
  (:export :make-supervisor))

(in-package :erlangen-platform.supervisor)

(defvar *maximum-intensity*) (defvar *restarts*) (defvar *start*)

(defun receive-exit ()
  (loop do (match (receive)
             ;; See: https://github.com/m2ym/optima/issues/115
             ((list* (and (type agent) agent)
                     (and (or :ok :exit) status)
                     values)
              (return-from receive-exit (values agent status values))))))

(defun check-restart-intensity ()
  (incf *restarts*)
  (let ((runtime (- (get-universal-time) *start*)))
    (unless (and (> runtime 0)
                 (< (/ *restarts* runtime) *maximum-intensity*))
    (error "Children are being restarted too often."))))

(defun one-for-one (childspecs)
  (let (child-function
        child-agent
        (agent-child (make-hash-table)))
    ;; Initiate by starting children.
    (loop for (id function) in childspecs
          for agent = (spawn function :attach :monitor)
       do
         (setf (getf child-function id) function)
         (setf (getf child-agent id) agent)
         (setf (gethash agent agent-child) id))
    ;; Loop over messages, restarting dead children; panic if maximum
    ;; restart intensity is exceeded.
    (loop for dead-agent = (receive-exit)
          for id = (gethash dead-agent agent-child)
       do
         (check-restart-intensity)
         (let ((agent (spawn (getf child-function id) :attach :monitor)))
           (remhash dead-agent agent-child)
           (setf (gethash agent agent-child) id)
           (setf (getf child-agent id) agent)))))

(defun find-supervisor (strategy)
  (ecase strategy
    (:one-for-one 'one-for-one)))

(defun make-supervisor (childspecs &key (strategy :one-for-one)
                                        (intensity 1)
                                        (period 5))
  "DOCUMENT ME! I AM PUBLIC!"
  (check-type intensity (integer 0))
  (check-type period (integer (0)))
  (loop for (id function) in childspecs do
       (check-type id keyword)
       (check-type function function))
  (lambda ()
    (let ((*maximum-intensity* (/ intensity period))
          (*restarts* 0)
          (*start* (get-universal-time)))
      (funcall (find-supervisor strategy) childspecs))))
