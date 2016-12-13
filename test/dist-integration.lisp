(spawn 'node)

;; Precondition: on the local host there is a dedicated instrumentation node
;; `dist-integration', registered as :INSTRUMENT, and of course
;; erlangen-port-mapper must be running.
(defparameter *instrument*
  (format nil "~a/dist-integration/:INSTRUMENT" (machine-instance)))

;; let’s wait a while for *INSTRUMENT* to come up.
(sleep 5)

(let ((echo (spawn 'receive :node "dist-integration" :attach :monitor)))
  (send '(:downtime 5) *instrument*) ; cause a downtime
  (sleep 2) ; wait a while for the remote to shut down
  (send :foo echo) ; this will be lost
  (send :bar echo) ; XXX - need two SEND to detect connection failure
  (sleep 5) ; wait until dist-integration comes up
  (send :baz echo) ; this should go through
  (assert (equal (receive :timeout 5)
                 `(,echo :ok :baz))))

(quit)
