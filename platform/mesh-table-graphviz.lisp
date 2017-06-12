(in-package :erlangen-platform.mesh-table)

(export '(make-dump-routes-request graphviz))

(defstruct (dump-routes-request (:include request)))
(defstruct (dump-routes-reply (:include reply)) routes)

(defmethod handle ((request dump-routes-request))
  (respond (make-dump-routes-reply
            :routes (reduce 'append (node-buckets *node*) :key 'bucket-routes))
           request))

(defstruct (hsv (:constructor hsv (h s v)))
  h s v)

(defun hsv-to-rgb (hsv)
  (with-slots (h s v) hsv
    (let* ((h-i (floor (* h 6)))
           (f (- (* h 6) h-i))
           (p (* v (- 1 s)))
           (q (* v (- 1 (* f s))))
           (tt (* v (- 1 (* (- 1 f) s)))))
      (ecase h-i
        (0 (values v tt p))
        (1 (values q v p))
        (2 (values p v tt))
        (3 (values p q v))
        (4 (values tt p v))
        ((5 6) (values v p q))))))

(defun integer-scale (f i)
  (round (* i f)))

(defun web-color (hsv)
  (multiple-value-bind (r g b) (hsv-to-rgb hsv)
    (format nil "#~2,'0X~2,'0X~2,'0X"
            (integer-scale r 255)
            (integer-scale g 255)
            (integer-scale b 255))))

(defun id-color (id)
  (hsv (/ id (1- (expt 2 *key-size*))) 0.42 0.98))

(defun distance-color (relative-distance)
  (hsv 0.6 0.1 (+ 0.4 (* 0.4 relative-distance))))

(defun distance-length (relative-distance)
  (* 5 relative-distance))

(defun relative-distance (from to)
  (/ (distance from to) (1- (expt 2 *key-size*))))

(defun find-forward (route trace)
  (find route trace :key 'fifth))

(defun trace-color (message)
  (values (id-color (slot-value (fourth message) 'key)) 1 1))

(defun graphviz (dumped-routes &key trace include-stale-p)
  (format t "digraph {
    layout=fdp; splines=true; overlap=scalexy;
    node [ width=0.4, height=0.4, style=filled, color=lightgrey, label=\"\" ];
    edge [ arrowsize=0.5 ];~%")
  (loop for reply in dumped-routes do
       (with-slots (id routes) reply
         (format t "~D [color=~S];~%" id (web-color (id-color id)))
         (loop for route in routes do
              (unless (and (route-stale-p route) (not include-stale-p))
                (let ((distance (relative-distance id (route-id route))))
                  (format t "~D -> ~D [color=~S, len=~F, style=~A]~%"
                          id
                          (route-id route)
                          (web-color (distance-color distance))
                          (distance-length distance)
                          (if (route-stale-p route)
                              "dotted"
                              "solid")))))))
  (loop for reply in dumped-routes do
       (with-slots (id routes) reply
         (loop for route in routes
               for message = (find-forward route trace)
            when message do
              (format t "~D -> ~D [color=~S, len=~F, penwidth=3, style=dashed]~%"
                      id
                      (route-id route)
                      (web-color (trace-color message))
                      (distance-length
                       (relative-distance id (route-id route)))))))
  (format t "}~%"))
