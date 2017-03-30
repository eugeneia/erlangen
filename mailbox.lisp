;;;; FIFO queue with two “lanes” used for message passing. The messages lane is
;;;; bounded while the priority lane is unbounded and takes precedence on read.

(in-package :erlangen.mailbox)

(defstruct (mailbox (:constructor make-mailbox%))
  "Mailbox structure."
  (messages (error "MESSAGES must be supplied."))
  (priority (error "PRIORITY must be supplied."))
  (enqueued (make-semaphore))
  (open-p t)
  (messages-dequeued 0)
  (messages-dropped 0))

(defun make-mailbox (size)
  "Return a new empty mailbox of SIZE."
  (make-mailbox% :messages (make-bounded-queue size)
                 :priority (make-queue)))

(defun close-mailbox (mailbox)
  "Close MAILBOX."
  (setf (mailbox-open-p mailbox) nil))

(defun enqueue-message (message mailbox)
  "Attempt to enqueue MESSAGE in MAILBOX."
  (with-slots (messages enqueued open-p) mailbox
    (if (and open-p (bounded-queue-push messages message))
        (signal-semaphore enqueued)
        (deltaf (mailbox-messages-dropped mailbox) '1+))))

(defun enqueue-priority (message mailbox)
  "Enqueue priority MESSAGE in MAILBOX."
  (with-slots (priority enqueued open-p) mailbox
    (cond (open-p (queue-push priority (list message))
                  (signal-semaphore (mailbox-enqueued mailbox)))
          (t      (deltaf (mailbox-messages-dropped mailbox) '1+)))))

(defun dequeue-message (mailbox &key timeout)
  "Return the next message in MAILBOX. Blocks depending on TIMEOUT. Only one
process (the “owner”) may call DEQUEUE-MESSAGE on a given `mailbox'."
  (with-slots (messages priority enqueued) mailbox
    (case timeout
      ;; TIMEOUT = nil: wait for new message indefinitely
      ((nil)     (wait-on-semaphore enqueued))
      ;; TIMEOUT = 0, signal `timeout' immediately
      (0         (unless (try-semaphore enqueued)
                   (error 'timeout)))
      ;; TIMEOUT = n: wait up to n seconds for new message
      (otherwise (unless (timed-wait-on-semaphore enqueued timeout)
                   (error 'timeout))))
    (deltaf (mailbox-messages-dequeued mailbox) '1+)
    (let ((priority-node (queue-pop priority)))
      (if priority-node
          (prog1 #1=(car priority-node)
            (setf #1# nil))
          (bounded-queue-pop messages)))))
