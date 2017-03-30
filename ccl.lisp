;;;; Extensions to Clozure CL used by Erlangen.

(in-package :ccl)

(set-development-environment)

(defun try-semaphore (s &optional flag)
  "Decrement semaphore S if possible. Returns T if S was decremented and NIL
otherwise."
  (%wait-on-semaphore-ptr (semaphore-value s) 0 0 flag))

(export 'try-semaphore)

;; Compare and swap, adapted from ChanL’s trivial-cas
(defmacro cas (place old new)
  "Atomic compare and swap. Atomically replace OLD with NEW in PLACE."
  (cond ((and (listp place) (eq (first place) 'car))
         `(%rplaca-conditional ,(second place) ,old ,new))
        ((and (listp place) (eq (first place) 'cdr))
         `(%rplacd-conditional ,(second place) ,old ,new))
        (t `(conditional-store ,place ,old ,new))))

(export 'cas)

(defmacro xchg (place new)
  "Atomic exchange. Atomically set PLACE to NEW value and return previous
value of PLACE."
  `(loop for old = ,place until (cas ,place old ,new)
      finally (return old)))

(export 'xchg)

(defmacro deltaf (place delta-function)
  `(loop for old = ,place
         for new = (funcall ,delta-function old)
      until (cas ,place old new)
      finally (return new)))

(export 'deltaf)

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
