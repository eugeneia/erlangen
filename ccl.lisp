;;;; Extensions to Clozure CL used by Erlangen.

(in-package :ccl)

(set-development-environment)

(defun try-semaphore (s &optional flag)
  "Decrement semaphore S if possible. Returns T if S was decremented and NIL
otherwise."
  (%wait-on-semaphore-ptr (semaphore-value s) 0 0 flag))

(export 'try-semaphore)

;; Compare and swap
(defmacro conditional-store (place old-value new-value &environment env)
  (setq place (macroexpand place env))
  (if (atom place)
    ;; CLX uses special variables' value cells as place forms.
    (if (and (symbolp place)
             (eq :special (ccl::variable-information place env)))
      (let* ((base (gensym))
             (offset (gensym)))
        `(multiple-value-bind (,base ,offset)
          (ccl::%symbol-binding-address ',place)
          (ccl::%store-node-conditional ,offset ,base ,old-value ,new-value)))
      (signal-program-error "~s is not a special variable ." place))
    (let* ((sym (car place))
           (struct-transform (structref-info sym env)))
      (if struct-transform
        (setq place (defstruct-ref-transform struct-transform (cdr place) env)
              sym (car place)))
      (if (eq (car place) 'the)
        (setq place (caddr place)
              sym (car place)))
      (case sym
        ((svref ccl::%svref ccl::struct-ref)
         (let* ((v (gensym)))
           `(let* ((,v ,(cadr place)))
              (ccl::store-gvector-conditional
               ,(caddr place) ,v ,old-value ,new-value))))
        (car
         `(%rplaca-conditional ,(cadr place) ,old-value ,new-value))
        (cdr
         `(%rplacd-conditional ,(cadr place) ,old-value ,new-value))
        (otherwise
         (signal-program-error "Don't know how to do conditional store to ~s" place))))))

(defmacro cas (place old new) ; alias CONDITIONAL-STORE to shorthand CAS
  `(conditional-store ,place ,old ,new))

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
