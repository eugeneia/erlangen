(defpackage erlangen-platform.timer
  (:use :cl :erlangen :erlangen-platform.log)
  (:export :timer))

(in-package :erlangen-platform.timer)

(defvar *ticks*) ; ( TICKS . ((tick . timers...) ...))

;;; (cdr   <ticks>) - ticks
;;; (cadr  <ticks>) - next ticklist
;;; (caadr <ticks>) - next tick

(defun find-or-insert-ticklist% (tick ticks)
  (cond ((or (null (cdr ticks)) (> (caadr ticks) tick))
         (let ((ticklist (list tick)))
           (setf (cdr ticks) (cons ticklist (cdr ticks)))
           ticklist))
        ((= (caadr ticks) tick)
         (cadr ticks))
        (t
         (find-or-insert-ticklist% tick (cdr ticks)))))

(defun find-or-insert-ticklist (tick)
  (if (cdr *ticks*)
      (find-or-insert-ticklist% tick *ticks*)
      (let ((ticklist (list tick)))
        (push ticklist (cdr *ticks*))
        ticklist)))

(defun insert-timer (timer tick)
  (write-log* (list* :set timer))
  (push timer (cdr (find-or-insert-ticklist tick))))

(defun pop-timers (tick)
  (loop while (and (cdr *ticks*) (<= (caadr *ticks*) tick))
        for (start . timers) = (pop (cdr *ticks*)) do
       (loop for timer in timers do
            (write-log* (list* :emit tick timer))
            (destructuring-bind (event agent &key repeat &allow-other-keys)
                timer
              (ignore-errors (send event agent))
              (when repeat
                (insert-timer timer (+ start repeat)))))))

(defun receive-timers (time-function)
  (loop for timer = (if (cdr *ticks*)
                        (ignore-errors (receive :timeout 0))
                        (receive))
     while timer do
       (ignore-errors
         (destructuring-bind (event agent &key start repeat) timer
           (declare (ignore event agent))
           (check-type start (or null (integer 0)))
           (check-type repeat (or null (integer 1)))
           (insert-timer timer
                         (or start
                             (if repeat
                                 (+ (funcall time-function) repeat)
                                 (funcall time-function))))))))

(defun timer (&key (time-function 'get-universal-time)
                   (sleep-function 'sleep)
                   (max-sleep 1)
                   log)
  (let ((*ticks* (list 'ticks))
        (*log* log))
    (loop do
         (pop-timers (funcall time-function))
         (receive-timers time-function)
         (funcall sleep-function
                  (min (- (caadr *ticks*) (funcall time-function))
                       max-sleep)))))
