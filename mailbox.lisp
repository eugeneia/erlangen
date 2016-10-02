;;;; Mailbox FIFO queue implementation based on JPL-QUEUES.

(in-package :erlangen.mailbox)

(defstruct (mailbox (:constructor make-mailbox%))
  "Mailbox structure."
  (queue (error "QUEUE must be supplied.") :type bounded-fifo-queue)
  (lock (make-lock "erlangen.mailbox"))
  (enqueued (make-semaphore))
  (dequeued (make-semaphore)))

(defun make-mailbox (size)
  "Return a new empty mailbox of SIZE."
  (make-mailbox%
   :queue (make-instance 'bounded-fifo-queue :capacity size)))

(define-condition mailbox-full (error) ()
  (:documentation
   "Describes an error condition that can occur when calling
    ENQUEUE-MESSAGE. It denotes a that the user attempted to enqueue a
    message into a full MAILBOX."))

(defun enqueue-message (message mailbox)
  "Attempt to enqueue MESSAGE in MAILBOX. If MAILBOX is full signal an
error of type MAILBOX-FULL."
  (with-slots (queue lock enqueued dequeued) mailbox
    (with-lock-grabbed (lock)
      (if (full? queue)
          (error 'mailbox-full)
          (progn (enqueue message queue)
                 (signal-semaphore enqueued)))))
  (values))

(defun empty-p (mailbox)
  "Predicate to test if MAILBOX is empty."
  (with-slots (queue lock) mailbox
    (with-lock-grabbed (lock)
      (empty? queue))))

(defun dequeue-message (mailbox)
  "Return the next message in MAILBOX. If MAILBOX is empty blocks until a
new message in enqueued."
  (with-slots (queue lock enqueued dequeued) mailbox
    (with-lock-grabbed (lock)
      (loop while (empty? queue) do
           (release-lock lock)
           (unwind-protect (wait-on-semaphore enqueued)
             (grab-lock lock)))
      (prog1 (dequeue queue)
        (signal-semaphore dequeued)))))
