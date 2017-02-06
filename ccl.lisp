;;;; Extensions to Clozure CL used by Erlangen.

(in-package :ccl)

(defun try-semaphore (s &optional flag)
  "Decrement semaphore S if possible. Returns T if S was decremented and NIL
otherwise."
  (%wait-on-semaphore-ptr (semaphore-value s) 0 0 flag))

(export 'try-semaphore)

(defmethod print-object ((o simple-error) stream)
  (print-unreadable-object (o stream :type t)
    (with-slots (format-control format-arguments) o
      (apply 'format stream format-control format-arguments))))

(defmethod print-object ((o socket-error) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~a: ~a"
            (socket-error-situation o)
            (or (socket-error-identifier o)
                (socket-error-code o)))))
