;;;; Generic supervisor agent.

(defpackage erlangen-platform.supervisor
  (:use :cl :erlangen :erlangen-platform.log)
  (:export :supervisor
           :one-for-one
           :one-for-all
           :rest-for-one))

(in-package :erlangen-platform.supervisor)

(defstruct (child (:constructor make-child%))
  id function restart spawn-args agent register-p)

(defun make-child (childspec)
  (destructuring-bind (id function
                          &key spawn-args register-p (restart :permanent))
      childspec
    (check-type id keyword)
    (check-type function (or function (and symbol (satisfies fboundp)) call))
    (check-type restart (member :permanent :transient :temporary))
    (check-type spawn-args list)
    (make-child% :id id
                 :function function
                 :spawn-args spawn-args
                 :register-p (not (null register-p))
                 :restart restart)))

(defun start-child (child)
  (with-slots (id function spawn-args register-p agent) child
    (write-log* `(,id :start))
    (setf agent (apply 'spawn function :attach :monitor spawn-args))
    (when register-p
      (register id :agent agent :supersede t)))
  child)

(defun stop-child (child reason)
  (with-slots (id agent) child
    (write-log* `(,id :stop))
    (exit reason agent)))

(defun restartable-p (child status)
  (case (child-restart child)
    (:permanent t)
    (:transient (eq status :exit))))

(defvar *maximum-intensity*) (defvar *restarts*) (defvar *period*)

(defun add-restart (restart restarts now period)
  (when (>= restart (- now period))
    (cons restart
          (when restarts
            (add-restart (car restarts) (cdr restarts) now period)))))

(defun check-restart-intensity ()
  (let ((now (get-internal-real-time)))
    (setf *restarts* (add-restart now *restarts* now *period*))
    (unless (<= (length *restarts*) *maximum-intensity*)
      (error "Children are being restarted too often."))))

(defun supervisor (childspecs &key (strategy 'one-for-one)
                                   (intensity 1)
                                   (period 5)
                                   log)
  "DOCUMENT ME! I AM PUBLIC!"
  (check-type intensity (integer 0))
  (check-type period (integer 1))
  (let* ((*maximum-intensity* intensity)
         (*period* (* period internal-time-units-per-second))
         (*restarts* ())
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
