;;;; Extensions to Clozure CL used by Erlangen.

(in-package :ccl)

(defun try-semaphore (s &optional flag)
  "Decrement semaphore S if possible. Returns T if S was decremented and NIL
otherwise."
  (%wait-on-semaphore-ptr (semaphore-value s) 0 0 flag))

(export 'try-semaphore)

(defmethod print-object ((o socket-error) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~a: ~a"
            (socket-error-situation o)
            (or (socket-error-identifier o)
                (socket-error-code o)))))

;; Add REUSE-PORT option to socket layer...

(defconstant SO_REUSEPORT 15)

(handler-bind
       ((simple-error
         #'(lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'continue))))
(defun set-socket-options (socket
                           &key 
                             keepalive
                             reuse-address
                             reuse-port
                             nodelay
                             broadcast
                             linger
                             (address-family :internet)
                             local-port
                             local-host
                             local-address
                             local-filename
                             type
                             connect
                             out-of-band-inline
			   &allow-other-keys)
  ;; see man socket(7) tcp(7) ip(7)
  (let ((fd (socket-device socket)))
    (when keepalive
      (int-setsockopt fd #$SOL_SOCKET #$SO_KEEPALIVE 1))
    (when reuse-address
      (int-setsockopt fd #$SOL_SOCKET #$SO_REUSEADDR 1))
    (when reuse-port
      (int-setsockopt fd #$SOL_SOCKET SO_REUSEPORT 1))
    (when broadcast
      (int-setsockopt fd #$SOL_SOCKET #$SO_BROADCAST 1))
    (when out-of-band-inline
      (int-setsockopt fd #$SOL_SOCKET #$SO_OOBINLINE 1))
    (when (member address-family '(:internet :internet6))
      (when (eq type :stream)
	(rlet ((plinger :linger))
	  (setf (pref plinger :linger.l_onoff) (if linger 1 0)
		(pref plinger :linger.l_linger) (or linger 0))
	  (socket-call socket "setsockopt"
		       (c_setsockopt fd #$SOL_SOCKET #$SO_LINGER
				     plinger (record-length :linger)))))
      (when nodelay
	(int-setsockopt fd
			#+linux-target #$SOL_TCP
			#-linux-target #$IPPROTO_TCP
			#$TCP_NODELAY 1))
      (when (or local-port local-host local-address)
        (socket-bind-local socket (or local-address
                                      (resolve-address :host local-host
                                                       :port local-port
                                                       :connect connect
                                                       :address-family address-family
                                                       :socket-type type)))))
    (when (and (eq address-family :file)
	       (eq connect :passive))
      (unless local-filename
        (error "need :local-filename argument to create passive file socket"))
      #+windows-target (error "can't create file socket on Windows")
      #-windows-target (socket-bind-local socket (make-instance 'unix-socket-address :path local-filename)))))

(defun make-socket (&rest keys
		    &key
                      (address-family :internet)
                      (type :stream)
                      (connect :active)
                      remote-host remote-port remote-address
                      eol format
                      keepalive reuse-address reuse-port nodelay broadcast linger
                      local-port local-host local-address
                      backlog class out-of-band-inline
                      local-filename remote-filename sharing basic
                      external-format (auto-close t)
                      connect-timeout input-timeout output-timeout deadline
                      fd)
  "Create and return a new socket."
  (declare (dynamic-extent keys))
  (declare (ignore type connect remote-host remote-port remote-address
		   eol format keepalive reuse-address reuse-port nodelay
		   broadcast linger local-port local-host
		   local-address backlog class out-of-band-inline
		   local-filename remote-filename sharing basic
		   external-format auto-close connect-timeout
		   input-timeout output-timeout deadline fd))
  (ecase address-family
    #-windows-target
    ((:file) (apply #'make-file-socket keys))
    ((:internet :internet6) (apply #'make-ip-socket keys))))
)
