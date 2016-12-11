;;;; Basic tests for agents.

(defpackage erlangen.agent-test
  (:use :cl
        :erlangen.agent
        :erlangen.conditions
        :erlangen.mailbox)
  (:export :run-tests
           :basic-message-benchmark))

(in-package :erlangen.agent-test)

(defmacro with-pseudo-agent ((var &key (mailbox-size
                                        '*default-mailbox-size*))
                             &body body)
  `(let* ((,var (make-instance 'erlangen.agent::agent
                               :mailbox (make-mailbox ,mailbox-size)))
          (erlangen.agent::*agent* ,var))
     ,@body))

(defun test-send-receive ()
  (with-pseudo-agent (p)
    (let ((relay (spawn (lambda () (send (receive) p)))))
      (spawn (lambda () (send :hello relay))))
    (assert (eq :hello (receive :timeout 1)) nil "SEND/RECEIVE failed.")
    (let ((timeout (spawn (lambda () (receive :timeout 1/10))
                          :attach :monitor)))
      (destructuring-bind (agent exit . error) (receive :timeout 2/10)
        (assert (and (eq agent timeout)
                     (eq exit :exit)
                     (typep error 'timeout))
                nil "RECEIVE (timeout) misbehaves.")))))

(defun test-monitor-kill ()
  (with-pseudo-agent (p)
    (let ((monitored (spawn (lambda () (receive)) :attach :monitor)))
      (exit :kill monitored)
      (assert (equal `(,monitored :exit . :kill) (receive :timeout 1)) nil
              "Monitor received corrupted message."))))

(defun test-link ()
  (with-pseudo-agent (p)
    (let ((parent (spawn (lambda ()
                           (spawn (lambda () (+ 1 2 3)) :attach :link)
                           (receive))
                         :attach :monitor)))
      (assert (equal `(,parent :exit :ok 6) (receive :timeout 1)) nil
              "Monitor received corrupt message."))))

(defun test-send-error-killed ()
  (with-pseudo-agent (p)
    (let ((agent (spawn (lambda () (receive)))))
      (exit :kill agent)
      (send :test agent)
      (handler-case (exit :test agent)
        (error (error)
          (error "EXIT to exited agent signals: ~a" error))))))

(defun run-tests ()
  (test-send-receive)
  (test-monitor-kill)
  (test-link)
  (test-send-error-killed)
  :ok)

(defun ping-pong-bench (n-messages &optional (timeout 60))
  (lambda ()
    (let ((start (get-internal-real-time))
          (n/2 (/ n-messages 2))
          (incrementer (spawn (lambda ()
                                (loop do
                                     (destructuring-bind (a . i)
                                         (receive)
                                       (send (1+ i) a)))))))
      (spawn (lambda ()
               (send (cons (agent) 0) incrementer)
               (loop for i = (receive) while (< i n/2)
                  do (send (cons (agent) i) incrementer)))
             :attach :monitor)
      (unwind-protect
           (destructuring-bind (spammer status result)
               (receive :timeout timeout)
             (declare (ignore spammer result))
             (let ((seconds (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second)))
               (assert (eq status :ok) nil "SPAMMER exited abnormally.")
               seconds))
        (exit :kill incrementer)))))

(defun basic-message-benchmark (&key (n-messages 1e6)
                                     (n-pairs 1)
                                     (timeout 60))
  (with-pseudo-agent (p)
    (loop repeat n-pairs do
         (spawn (ping-pong-bench n-messages timeout) :attach :monitor))
    (let ((total-messages (* n-pairs n-messages))
          (seconds (loop repeat n-pairs
                      for (agent status seconds) = (receive)
                      do (assert (eq status :ok) nil
                                 "~a exited abnormally." agent)
                      maximize seconds)))
      (format t "Sent ~f million messages in ~f seconds.~%"
              (/ total-messages 1e6) seconds)
      (format t "That’s ~f messages per second.~%"
              (/ total-messages seconds)))))
