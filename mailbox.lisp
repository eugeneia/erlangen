;;;; Mailbox: MPSC FIFO queue used for message passing.

(in-package :erlangen.mailbox)

(defstruct (mailbox (:constructor make-mailbox%))
  "Mailbox structure."
  (queue (error "QUEUE must be supplied."))
  (size (error "SIZE must be supplied."))
  (enqueued (make-semaphore))
  (messages-dequeued 0)
  (messages-dropped 0))

(defun make-mailbox (size)
  "Return a new empty mailbox of SIZE."
  (check-type size (integer 1))
  (make-mailbox% :queue (make-queue) :size size))

(defun enqueue-message (message mailbox &optional priority-p)
  "Attempt to enqueue MESSAGE in MAILBOX."
  (handler-case (enqueue (mailbox-queue mailbox)
                         message
                         (unless (and priority-p (> (mailbox-size mailbox) 0))
                           (mailbox-size mailbox)))
    (:no-error (node)
      (declare (ignore node))
      (signal-semaphore (mailbox-enqueued mailbox)))
    (error (error)
      (declare (ignore error))
      (deltaf (mailbox-messages-dropped mailbox) '1+))))

(defun dequeue-message (mailbox &key timeout)
  "Return the next message in MAILBOX. Blocks depending on TIMEOUT. Only one
process (the “owner”) may call DEQUEUE-MESSAGE on a given `mailbox'."
  (case timeout
    ;; TIMEOUT = nil: wait for new message indefinitely
    ((nil)     (wait-on-semaphore (mailbox-enqueued mailbox)))
    ;; TIMEOUT = 0, signal `timeout' immediately
    (0         (unless (try-semaphore (mailbox-enqueued mailbox))
                 (error 'timeout)))
    ;; TIMEOUT = n: wait up to n seconds for new message
    (otherwise (unless (timed-wait-on-semaphore
                        (mailbox-enqueued mailbox) timeout)
                 (error 'timeout))))
  (prog1 (dequeue (mailbox-queue mailbox))
    (deltaf (mailbox-messages-dequeued mailbox) '1+)))

(defun close-mailbox (mailbox)
  "Close MAILBOX."
  (setf (mailbox-size mailbox) 0))
