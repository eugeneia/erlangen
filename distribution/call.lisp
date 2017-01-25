;;;; Erlangen portable function call.

(in-package :erlangen.distribution.call)

(defun proper-list-p (cons &aux (cdr (cdr cons)))
  (or (and (consp cdr) (proper-list-p cdr))
      (null cdr)))

(deftype call ()
  "*Syntax:*

   _call_::= {(}_function_ _argument_\\*{)}

   *Arguments and Values:*

   _function_—a _symbol_ denoting a _function_.

   _argument_—a _serializable object_.

   *Description:*

   A _call_ denotes a portable function call to be invoked on a given
   _node_. A _call_ is a _list_ whose first element is a _symbol_
   denoting a _function_ and whose remaining elements are arguments to be
   applied to the denoted _function_."
  '(and (cons (and symbol (not null))) (satisfies proper-list-p)))

(defun make-function (call)
  "Return function with no arguments which applies CALL."
  (check-type call call)
  (lambda ()
    (apply 'funcall call)))
