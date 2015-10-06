;;;; Mailbox FIFO queue implementation based on JPL-QUEUES.

(defpackage erlangen.mailbox
  (:use :cl
        :jpl-queues
        :bordeaux-threads)
  (:export :mailbox
           :make-mailbox
           :enqueue-message
           :empty-p
           :dequeue-message
           :close-mailbox
           :mailbox-full
           :mailbox-closed))

(in-package :erlangen.mailbox)

(defstruct (mailbox (:constructor make-mailbox%))
  "Mailbox structure."
  (queue (error "QUEUE must be supplied.") :type bounded-fifo-queue)
  (open? t :type symbol)
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
    message into a full MAILBOX."))

(define-condition mailbox-closed (error) ()
  (:documentation
   "Describes an error condition that can occur when calling
    ENQUEUE-MESSAGE. It denotes a that the user attempted to enqueue a
    message into a closed MAILBOX."))

(defun enqueue-message (message mailbox)
  "Attempt to enqueue MESSAGE in MAILBOX. If MAILBOX is full signal an
error of type MAILBOX-FULL."
  (with-slots (queue open? lock enqueued dequeued) mailbox
    (with-lock-held (lock)
      (cond ((not open?)
             (error 'mailbox-closed))
            ((full? queue)
             (error 'mailbox-full))
            (t
             (enqueue message queue)
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

(defun close-mailbox (mailbox)
  "Close MAILBOX."
  (with-lock-held ((mailbox-lock mailbox))
    (setf (mailbox-open? mailbox) nil)))
