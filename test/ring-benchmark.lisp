;;;; Classic ring benchmark for Erlangen.

(defpackage erlangen.ring-benchmark
  (:use :cl :erlangen)
  (:export :message-ring-benchmark))

(in-package :erlangen.ring-benchmark)

(defun ring-hop (next-hop)
  (loop for data = (receive)
        for (remaining-hops . start) = data
     if (> remaining-hops 0) do (decf (car data)) (send data next-hop)
     else return (- (get-internal-real-time) start)))

(defun message-ring (n-agents n-hops)
  (let ((next-hop (loop for i from 1 to n-agents
                        for next-hop = (agent)
                          then (spawn (lambda () (ring-hop next-hop))
                                      :attach :link)
                     finally (return next-hop))))
    (send (cons n-hops (get-internal-real-time)) next-hop)
    (ring-hop next-hop)))

(defun message-ring-benchmark (&key (n-agents 10)
                                    (n-hops 1000000)
                                    (timeout 60))
  (check-type n-agents (integer 2))
  (check-type n-hops (integer 1))
  (spawn (lambda () (message-ring n-agents n-hops)) :attach :monitor)
  (destructuring-bind (agent exit ok duration
                       &aux (seconds (/ duration
                                        internal-time-units-per-second)))
      (receive :timeout timeout)
    (declare (ignore agent exit ok))
    (format t "Traversed ~a hops in a ring of ~a agents in ~f seconds.~%"
            n-hops n-agents seconds)
    (format t "That’s ~f messages per second, or ~f μs per hop.~%"
            (/ n-hops seconds) (* 1000000 (/ seconds n-hops)))))

