;;;; Mailbox FIFO queue implementation based on JPL-QUEUES.

(defpackage erlangen.mailbox
  (:use :cl
        :jpl-queues
        :bordeaux-threads)
  (:export :make-mailbox
           :enqueue-message
           :empty-p
           :dequeue-message
           :mailbox-full))

(in-package :erlangen.mailbox)

(defstruct (mailbox (:constructor make-mailbox%))
  "Mailbox structure."
  (queue (error "QUEUE must be supplied.") :type bounded-fifo-queue)
  (lock (make-lock))
  (enqueued (make-condition-variable))
  (dequeued (make-condition-variable)))

(defun make-mailbox (size)
  "Return a new empty mailbox of SIZE."
  (make-mailbox%
   :queue (make-instance 'bounded-fifo-queue :capacity size)))

(define-condition mailbox-full (error) ()
  (:documentation
   "Describes an error condition that can occur when calling
    ENQUEUE-MESSAGE. It denotes a that the user attempted to enqueue a
    message into a full `mailbox'."))

(defun enqueue-message (message mailbox)
  "Attempt to enqueue MESSAGE in MAILBOX. If MAILBOX is full signal an
error of type `mailbox-full'."
  (with-slots (queue lock enqueued dequeued) mailbox
    (with-lock-held (lock)
      (if (full? queue)
          (error 'mailbox-full)
          (progn (enqueue message queue)
                 (condition-notify enqueued)))))
  (values))

(defun empty-p (mailbox)
  "Predicate to test if MAILBOX is empty."
  (with-slots (queue lock) mailbox
    (with-lock-held (lock)
      (empty? queue))))

(defun dequeue-message (mailbox)
  "Return the next message in MAILBOX. If MAILBOX is empty blocks until a
new message in enqueued."
  (with-slots (queue lock enqueued dequeued) mailbox
    (with-lock-held (lock)
      (loop while (empty? queue)
            doing (condition-wait enqueued lock))
      (prog1 (dequeue queue)
        (condition-notify dequeued)))))
