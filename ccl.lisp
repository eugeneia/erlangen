;;;; Extensions to Clozure CL used by Erlangen.

(in-package :ccl)

(defun try-semaphore (s &optional flag)
  "Decrement semaphore S if possible. Returns T if S was decremented and NIL
otherwise."
  (%wait-on-semaphore-ptr (semaphore-value s) 0 0 flag))

(export 'try-semaphore)
