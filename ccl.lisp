;;;; Extensions to Clozure CL used by Erlangen.

(in-package :ccl)

(set-development-environment)

(defun try-semaphore (s &optional flag)
  "Decrement semaphore S if possible. Returns T if S was decremented and NIL
otherwise."
  (%wait-on-semaphore-ptr (semaphore-value s) 0 0 flag))

(export 'try-semaphore)

(defmethod print-object ((o socket-error) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~a: ~a"
            (socket-error-situation o)
            (or (socket-error-identifier o)
                (socket-error-code o)))))

(defun get-internal-real-time ()
  (nth-value 0 (floor (current-time-in-nanoseconds)
                      (load-time-value
                       (/ 1000000000 internal-time-units-per-second)))))

(set-user-environment)
