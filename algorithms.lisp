;;;; Reusable algorithms.

(in-package :erlangen.algorithms)

(defun poll-timeout% (predicate timeout poll-interval success-fn fail-fn)
  (when (= timeout 0)
    (if (funcall predicate)
        (return-from poll-timeout% (funcall success-fn))
        (return-from poll-timeout% (funcall fail-fn))))
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

(defmacro with-poll-select (poll-interval &rest clauses)
  "CLAUSE::= (POLL-FORM (&rest VARS) &body body)

Poll every POLL-INTERVAL seconds until a CLAUSE's POLL-FORM returns true
as its first value. Evaluate the CLAUSE's BODY with VARS bound to
POLL-FORM's return values and return. If a CLAUSE's VARS are nil (empty),
the clause will repeatedly be polled but never cause a return and its
BODY will never be evaluated."
  `(block select
     (loop do
          ,@(loop for clause in clauses collect
                 (destructuring-bind (form &optional vars &rest body)
                     clause
                   `(multiple-value-bind ,vars ,form
                      (when ,(first vars)
                        (return-from select (progn ,@body))))))
          (sleep ,poll-interval))))
