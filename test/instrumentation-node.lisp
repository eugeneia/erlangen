(register :instrument)

(defparameter *node-call* '(node :name "dist-integration"))

(defvar *node* (spawn *node-call*))

(defun dformat (&rest args)
  (apply 'format *error-output* args)
  (finish-output *error-output*))

(loop for (command . args) = (receive) do
     (dformat "~a ~a~%" command args)
     (ecase command
       (:downtime
        (destructuring-bind (duration) args
          (exit 'downtime *node*)
          (sleep duration)
          (setf *node* (spawn *node-call*))
          (dformat "up!~%")))))
