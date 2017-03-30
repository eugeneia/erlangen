;;;; Erlangen queue arsenal, these queues are multi-producer, single-consumer
;;;; FIFO queues.

(in-package :erlangen.queues)

;;; Intrusive Vyukov queue:

(defstruct (queue (:constructor make-queue%))
  head tail)

(defun make-queue (&aux (stub (list nil)))
  (make-queue% :head stub :tail stub))

(defun queue-push (queue node)
  (setf (cdr (xchg (queue-head queue) node)) node))

(defun queue-pop (queue)
  (let* ((tail (queue-tail queue))
         (next (cdr tail)))
    (when next
      (setf (queue-tail queue) next))))

;;; Intrusive bounded queue, fails on overflow:

(defstruct (bounded-queue (:constructor make-bounded-queue%))
  buffer mask (read 0) (write 0))

(defstruct cell
  sequence value)

(defun make-bounded-queue (size)
  (check-type size (and (integer 2) fixnum))
  (assert (= (logand size (1- size)) 0))
  (let ((buffer (make-array size :element-type 'cell)))
    (dotimes (i size)
      (setf (svref buffer i) (make-cell :sequence i)))
    (make-bounded-queue% :buffer buffer :mask (1- size))))

(defun bounded-queue-push (queue value)
  (loop do (let* ((write (bounded-queue-write queue))
                  (next-write (logand (1+ write) (bounded-queue-mask queue)))
                  (cell (svref (bounded-queue-buffer queue) write))
                  (diff (- (cell-sequence cell) write)))
             (cond ((= diff 0)
                    (when (cas (bounded-queue-write queue) write next-write)
                      (setf (cell-value cell) value
                            (cell-sequence cell) next-write)
                      (return t)))    ; value pushed
                   ((< diff 0)
                    (return nil)))))) ; queue is full

(defun bounded-queue-pop (queue)
  (loop do (let* ((read (bounded-queue-read queue))
                  (next-read (logand (1+ read) #1=(bounded-queue-mask queue)))
                  (cell (svref (bounded-queue-buffer queue) read))
                  (diff (- (cell-sequence cell) next-read)))
             (cond ((= diff 0)
                    (setf (bounded-queue-read queue) next-read
                          (cell-sequence cell) (logand (+ next-read #1#) #1#))
                    (return (cell-value cell)))
                   ((< diff 0)
                    (error "Queue is empty."))))))
