(in-package :erlangen.queue)

(defstruct (queue (:constructor make-queue%))
  head tail (length 0))

(defun make-queue (&aux (stub (list 'end)))
  (make-queue% :head stub :tail stub))

(defun enqueue (queue x &optional limit &aux (node (list x)))
  (deltaf (queue-length queue)
          (lambda (length)
            (if (and limit (>= length limit))
                (error "Queue is full.")
                (1+ length))))
  (setf (cdr (xchg (queue-head queue) node)) node))

(defun dequeue (queue)
  (let* ((tail (queue-tail queue))
         (next (or (cdr tail) (error "Queue is empty."))))
    (setf (queue-tail queue) next)
    (deltaf (queue-length queue) '1-)
    (prog1 (car next)
      (setf (car next) 'end))))
