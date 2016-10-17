;;;; Mailbox FIFO queue implementation based on JPL-QUEUES.

(in-package :erlangen.mailbox)

(defstruct (mailbox (:constructor make-mailbox%))
  "Mailbox structure."
  (queue (error "QUEUE must be supplied.") :type bounded-fifo-queue)
  (priority (error "PRIORITY must be supplied.") :type unbounded-fifo-queue)
  (open? t :type symbol)
  (lock (make-lock "erlangen.mailbox"))
  (enqueued (make-semaphore)))

(defun make-mailbox (size)
  "Return a new empty mailbox of SIZE."
  (make-mailbox%
   :queue (make-instance 'bounded-fifo-queue :capacity size)
   :priority (make-instance 'unbounded-fifo-queue)))

(define-condition mailbox-full (error) ()
  (:documentation
   "Describes an error condition that can occur when calling
    ENQUEUE-MESSAGE. It denotes a that the user attempted to enqueue a
    message into a full MAILBOX."))

(define-condition mailbox-closed (error) ()
  (:documentation
   "Describes an error condition that can occur when calling
    ENQUEUE-MESSAGE. It denotes a that the user attempted to enqueue a
    message into a closed MAILBOX."))

(defun enqueue-message (message mailbox)
  "Attempt to enqueue MESSAGE in MAILBOX. If MAILBOX is full signal an
error of type MAILBOX-FULL, if its closed signal an error of type
MAILBOX-CLOSED."
  (with-slots (queue open? lock enqueued) mailbox
    (with-lock-grabbed (lock)
      (cond ((not open?)
             (error 'mailbox-closed))
            ((full? queue)
             (error 'mailbox-full))
            (t
             (enqueue message queue)
             (signal-semaphore enqueued)))))
  (values))

(defun enqueue-priority (message mailbox)
  "Attempt to enqueue priority MESSAGE in MAILBOX. Fails if MAILBOX is closed,
but does *not* signal an error."
  (with-slots (priority open? lock enqueued) mailbox
    (with-lock-grabbed (lock)
      (when open?
        (enqueue message priority)
        (signal-semaphore enqueued))))
  (values))

(defun empty-p (mailbox)
  "Predicate to test if MAILBOX is empty."
  (with-slots (queue priority lock) mailbox
    (with-lock-grabbed (lock)
      (and (empty? queue) (empty? priority)))))

(defun dequeue-message (mailbox)
  "Return the next message in MAILBOX. If MAILBOX is empty blocks until a
new message in enqueued."
  (with-slots (queue priority lock enqueued) mailbox
    (with-lock-grabbed (lock)
      (loop for queue-empty-p = (empty? queue)
            for priority-empty-p = (empty? priority)
         while (and queue-empty-p priority-empty-p) do
           (release-lock lock)
           (unwind-protect (wait-on-semaphore enqueued)
             (grab-lock lock))
         finally
           (return (if priority-empty-p
                       (dequeue queue)
                       (dequeue priority)))))))

(defun close-mailbox (mailbox)
  "Close MAILBOX."
  (with-lock-grabbed ((mailbox-lock mailbox))
    (setf (mailbox-open? mailbox) nil)))
