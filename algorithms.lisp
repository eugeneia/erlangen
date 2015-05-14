;;;; Reusable algorithms.

(defpackage erlangen.algorithms
  (:documentation "Reusable algorithms.")
  (:use :cl)
  (:export :with-poll-timeout))

(in-package :erlangen.algorithms)

(defun poll-timeout% (predicate timeout poll-interval success-fn fail-fn)
  (loop for elapsed = 0 then (+ elapsed poll-interval) do
       (cond ((> elapsed timeout)
              (return-from poll-timeout% (funcall fail-fn)))
             ((funcall predicate)
              (return-from poll-timeout% (funcall success-fn)))
             (t
              (sleep poll-interval)))))

(defmacro with-poll-timeout ((predicate-form &key timeout poll-interval)
                             &key succeed fail)
  "Poll every POLL-INTERVAL seconds until PREDICATE-FORM returns a true
value or TIMEOUT seconds have elapsed. In the former case evaluate and
return the result of SUCCEED. Otherwise evaluate and return the result of
FAIL."
  `(poll-timeout% (lambda () ,predicate-form) ,timeout ,poll-interval
                  (lambda () ,succeed)
                  (lambda () ,fail)))
