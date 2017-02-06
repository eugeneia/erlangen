;;;; Generic supervisor agent.

(defpackage erlangen-platform.supervisor
  (:use :cl :erlangen :erlangen-platform.log)
  (:export :supervisor
           :one-for-one
           :one-for-all
           :rest-for-one))

(in-package :erlangen-platform.supervisor)

(defstruct (child (:constructor make-child%))
  id function restart spawn-args agent)

(defun make-child (childspec)
  (destructuring-bind (id function &key (restart :permanent) spawn-args)
      childspec
    (check-type id keyword)
    (check-type function (or function (and symbol (satisfies fboundp)) call))
    (check-type restart (member :permanent :transient :temporary))
    (check-type spawn-args list)
    (make-child% :id id
                 :function function
                 :restart restart
                 :spawn-args spawn-args)))

(defun start-child (child)
  (with-slots (id function spawn-args agent) child
    (write-log* `(,id :start))
    (setf agent (apply 'spawn function :attach :monitor spawn-args)))
  child)

(defun stop-child (child reason)
  (with-slots (id agent) child
    (write-log* `(,id :stop))
    (exit reason agent)))

(defun restartable-p (child status)
  (case (child-restart child)
    (:permanent t)
    (:transient (eq status :exit))))

(defvar *maximum-intensity*) (defvar *restarts*) (defvar *start*)

(defun check-restart-intensity ()
  (incf *restarts*)
  (let ((runtime (- (get-universal-time) *start*)))
    (unless (and (> runtime 0) (< (/ *restarts* runtime) *maximum-intensity*))
      (error "Children are being restarted too often."))))

(defun supervisor (childspecs &key (strategy 'one-for-one)
                                   (intensity 1)
                                   (period 5)
                                   log)
  "DOCUMENT ME! I AM PUBLIC!"
  (check-type intensity (integer 0))
  (check-type period (integer 1))
  (let* ((*maximum-intensity* (/ intensity period))
         (*restarts* 0)
         (*start* (get-universal-time))
         (*log* log)
         (children (loop for childspec in childspecs collect
                        (start-child (make-child childspec)))))
    (loop for notice = (receive) do
         (destructuring-bind (agent status &rest values) notice
           (let ((child (find agent children :key 'child-agent)))
             (when child
               (write-log* (list* (child-id child) status values))
               (when (restartable-p child status)
                 (check-restart-intensity)
                 (funcall strategy children child notice))))))))

(defun one-for-one (children failed notice)
  (declare (ignore children notice))
  (start-child failed))

(defun one-for-all (children failed notice)
  (loop for child in children do
       (unless (eq child failed)
         (stop-child child notice))
       (start-child child)))

(defun rest-for-one (children failed notice)
  (loop with rest-p = nil
     for child in children
     when (eq child failed) do
       (start-child failed)
       (setf rest-p t)
     when rest-p do
       (stop-child child notice)
       (start-child child)))
