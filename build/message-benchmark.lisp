;;;; message-benchmark: basic message benchmark.

(require :asdf)

(asdf:load-system :erlangen)

(defun message-benchmark ()
  (when (intersection *command-line-argument-list*
                      '("-h" "-help" "--help")
                      :test 'string=)
    (princ "Usage: erlangen-message-benchmark [n-messages] [n-pairs] [timeout]
       erlangen-message-benchmark -h|-help|--help")
    (quit 0))
  (destructuring-bind (exe &optional n-messages n-pairs timeout)
      *command-line-argument-list*
    (declare (ignore exe))
    (apply 'erlangen.agent-test:basic-message-benchmark
           (append (when n-messages
                     `(:n-messages ,(parse-integer n-messages)))
                   (when n-pairs
                     `(:n-pairs ,(parse-integer n-pairs)))
                   (when timeout
                     `(:timeout ,(parse-integer timeout)))))))

(defclass erlangen-message-benchmark (ccl::application)
  ((command-line-arguments :initform nil)))

(setf ccl::*invoke-debugger-hook-on-interrupt* t)
(setf ccl::*debugger-hook*
      (lambda (condition hook)
        (declare (ignore hook))
        (etypecase condition
          (ccl::interrupt-signal-condition (quit 130)))))

(gc)

(save-application
 "bin/erlangen-message-benchmark"
 :application-class 'erlangen-message-benchmark
 :toplevel-function 'message-benchmark
 :error-handler :quit
 :purify t
 :prepend-kernel t)
