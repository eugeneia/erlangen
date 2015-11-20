;;;; Erlangen core macros.

(in-package erlangen.macros)

(defun receive-nowait ()
  "Non-blocking RECEIVE, returns two values: boolean indicating success
and message or nil."
  (multiple-value-bind (value error)
      (ignore-errors (receive :timeout 0))
    (values (null error) value)))

(defun parse-select-clauses (clauses)
  "Parse SELECT's clauses grammar and return regular clauses and
optionally :RECEIVE clause."
  (let ((butlast (butlast clauses))
        (last (last clauses)))
    (loop for clause in butlast do
         (when (eq (car clause) :receive)
           (error ":RECEIVE clause must be last.")))
    (if (eq (caar last) :receive)
        (values butlast (cdar last))
        (values clauses nil))))

(let ((message-p-sym (gensym "message-p")))
  (defmacro select (&rest clauses)
    "_clauses_::= 〚_normal-clause_\\*〛 \\[_receive-clause_]

     _normal-clause_::= {(}_poll-form_ _vars_ _body-form_\\*{)}

     _receive-clause_::= {(:receive} _vars_ _body-form_\\*{)}

     *Arguments and Values:*

     _poll-form_, _body-form_—_forms_.

     _vars_—a _list_ of _symbols_.

     *Description:*

     {select} repeatedly calls the _poll-forms_ of each _normal-clause_
     (in order) until a _poll-form_ returns a non-nil value as its first
     result and _vars_ is non-nil. It then evaluates each _body-form_ of
     the respective _normal-clause_ with the return values of its
     _poll-forms_ bound to _vars_ and returns their result.

     If a _receive-clause_ is supplied and its _vars_ are non-nil,
     {select} will evaluate each _body-form_ of the clause with the
     received message bound to the first _symbol_ in _vars_ and return
     their result. If no _receive-clause_ is supplied, {select} will
     silently discard incoming messages.

     *Exceptional Situations:*

     If {select} is not called by an _agent_ an _error_ of _type_
     {type-error} is signaled."
    (multiple-value-bind (clauses receive-clause)
        (parse-select-clauses clauses)
      `(progn
         (check-type *agent* agent)
         (with-poll-select 1e-2
           ,@clauses
           ,(if receive-clause
                (destructuring-bind (vars &rest body) receive-clause
                  `((receive-nowait) (,message-p-sym ,vars)
                    ,@body))
                '((receive-nowait) ())))))))

