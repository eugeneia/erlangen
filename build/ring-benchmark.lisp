;;;; ring-benchmark: classic ring benchmark.

(require :asdf)

(asdf:load-system :erlangen)

(defun ring-benchmark ()
  (when (intersection *command-line-argument-list*
                      '("-h" "-help" "--help")
                      :test 'string=)
    (princ "Usage: erlangen-ring-benchmark [n-hops] [n-agents] [timeout]
       erlangen-ring-benchmark -h|-help|--help")
    (quit 0))
  (destructuring-bind (exe &optional n-hops n-agents timeout)
      *command-line-argument-list*
    (declare (ignore exe))
    (apply 'erlangen.ring-benchmark:message-ring-benchmark
           (append (when n-hops
                     `(:n-hops ,(parse-integer n-hops)))
                   (when n-agents
                     `(:n-agents ,(parse-integer n-agents)))
                   (when timeout
                     `(:timeout ,(parse-integer timeout)))))))

(defclass erlangen-ring-benchmark (ccl::application)
  ((command-line-arguments :initform nil)))

(setf ccl::*invoke-debugger-hook-on-interrupt* t)
(setf ccl::*debugger-hook*
      (lambda (condition hook)
        (declare (ignore hook))
        (etypecase condition
          (ccl::interrupt-signal-condition (quit 130)))))

(gc)

(save-application
 "bin/erlangen-ring-benchmark"
 :application-class 'erlangen-ring-benchmark
 :toplevel-function 'ring-benchmark
 :error-handler :quit
 :purify t
 :prepend-kernel t)
