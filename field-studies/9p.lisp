;;;; Implementation of the 9P2000 protocol.

(defpackage nodes.9p ;; Aka `neun', spin off as library.
  (:use :cl :bytes :flexi-streams))

(in-package :nodes.9p)


(defun writer-symbol (symbol)
  (intern (format nil "WRITE-~a" (symbol-name symbol)) :nodes.9p))

(defun reader-symbol (symbol)
  (intern (format nil "READ-~a" (symbol-name symbol)) :nodes.9p))

(defun assert-size (name type size bytes)
  (unless (< size (expt 2 (* 8 bytes)))
    (error 'type-error
           :format-control "~a does not fit type ~a."
           :datum name
           :expected-type type)))

(defun compile-write-integer (name type size)
  `(progn
     ;; Assert that DATUM fits into SIZE bytes.
     (assert-size ',name ',type datum ,size)
     (write-bytes datum ,size stream)))

(defun compile-9p-string-writer (name type bytes)
  `(progn
     ;; Assert that (length DATUM) fits into BYTES.
     (assert-size ',name ',type (length datum) ,bytes)
     ;; size[bytes]
     (write-bytes (length datum) ,bytes stream)
     ;; string
     (write-sequence datum stream)))

(defun compile-write-vector (name type element-type)
  `(progn
     ;; Assert that (length DATUM) fits into 2 bytes.
     (assert-size ',name ',type (length datum) 2)
     ;; s[2]
     (write-bytes (length datum) 2 stream)
     (loop for element in datum do
          (,(writer-symbol element-type) element stream))))

(defun compile-read-integer (size)
  `(read-bytes stream ,size))

(defun compile-9p-string-reader (bytes)
  `(let* (; Read size[bytes]
          (size (read-bytes stream ,bytes))
          (buffer (make-array size :element-type '(unsigned-byte 8))))
     (unless (= (read-sequence buffer stream) size)
       (error 'end-of-file :stream stream))
     buffer))

(defun compile-read-vector (element-type)
  `(let (;Read s[2]
         (size (read-bytes stream 2)))
     (loop for i from 1 to size collect
          (,(reader-symbol element-type) stream))))

(defmacro define-datum (type name spec)
  ;; Introspection
  (setf (get name :datum-type) type
        (get name :datum-spec) spec)
  `(progn
     ;; Writer
     (defun ,(writer-symbol name) (datum stream)
       ,(ecase type
          (integer (compile-write-integer    name type spec))
          (string  (compile-9p-string-writer name type spec))
          (vector  (compile-write-vector     name type spec)))
       (values))
     ;; Reader
     (defun ,(reader-symbol name) (stream)
       ,(ecase type
          (integer (compile-read-integer     spec))
          (string  (compile-9p-string-reader spec))
          (vector  (compile-read-vector      spec))))))


;; Data definitions.
(define-datum integer size      4)
(define-datum integer type      1)
(define-datum integer tag       2)
(define-datum string  name      2)
(define-datum vector  names  name)
(define-datum integer fid       4)
(define-datum integer qid      13)
(define-datum vector  qids    qid)
(define-datum integer mode      1)
(define-datum integer perm      4)
(define-datum integer offset    8)
(define-datum string  data      4)
(define-datum string  stat      2)


(defvar *message-size* nil
  "Set to `msize' during `session'.")

(defparameter *message-table*
  (make-hash-table :test 'eql)
  "Lookup table for integer=>symbol.")

(defun message-type (name)
  (get name :message-type))

(defun type-symbol (type)
  (gethash type *message-table*))

(defun compile-compute-9p-string-size (name type)
  `(+ ,(get type :datum-spec) (length ,name)))

(defun compile-compute-vector-size (name type)
  `(+ 2 ; `length'
      ,(let ((subdatum (get type :datum-spec)))
            (ecase (get subdatum :datum-type)
              (integer `(* ,(get subdatum :datum-spec) (length ,name)))
              (string  `(loop for element in ,name sum
                             ,(compile-compute-9p-string-size
                               'element subdatum)))))))

(defun compile-compute-size (fields)
  `(+ ,(get 'size :datum-spec)
      ,(get 'type :datum-spec)
      ,(get 'tag  :datum-spec)
      ,@(loop for (name type) in fields collect
             (ecase (get type :datum-type)
               (integer (get type :datum-spec))
               (string  (compile-compute-9p-string-size name type))
               (vector (compile-compute-vector-size name type))))))

(defun assert-message-size (size)
  (when (> size *message-size*)
    (error "Message too long to fit negotiated `msize'.")))

(defun write-message-size (size stream)
  (assert-message-size size)
  (write-size size stream))

(defun compile-write-message (name fields)
  `(progn (write-message-size ,(compile-compute-size fields) stream)
          (write-type ,(message-type name) stream)
          (write-tag tag stream)
          ,@(loop for (name type) in fields collect
                 `(,(writer-symbol type) ,name stream))))

(let ((header-size (+ (get 'size :datum-spec) (get 'type :datum-spec))))
  (defun read-message (stream)
    (let ((size (read-size stream)))
      (assert-message-size size)
      (values
       (type-symbol (read-type stream))
       (let* ((buffer-size (- size header-size))
              (buffer (make-array buffer-size
                                  :element-type '(unsigned-byte 8))))
         (unless (= (read-sequence buffer stream) buffer-size)
           (error 'end-of-file :stream stream))
         buffer)))))

(defun compile-read-message (fields)
  `(with-input-from-sequence (stream buffer)
     (values (read-tag stream)
             ,@(loop for type in (mapcar 'second fields) collect
                    `(,(reader-symbol type) stream)))))

(defmacro define-message (type name &rest fields)
  (setf (get name :message-type) type
        (gethash type *message-table*) name)
  `(progn
     (defun ,(writer-symbol name) (tag ,@(mapcar 'first fields) stream)
       ,(compile-write-message name fields))
     (defun ,(reader-symbol name) (buffer)
       ,(compile-read-message fields))
     (values)))


;; Tversion msize[4] version[s]
;; Rversion msize[4] version[s]
(define-message 100 version-request (msize size) (version name))
(define-message 101 version-reply   (msize size) (version name))

;; Tauth afid[4] uname[s] aname[s]
;; Rauth aqid[13]
(define-message 102 auth-request (afid fid) (uname name) (aname name))
(define-message 103 auth-reply (aqid qid))

;; Tattach fid[4] afid[4] uname[s] aname[s]
;; Rattach qid[13]
(define-message 104 attach-request (fid fid) (afid fid) (uname name) (aname name))
(define-message 105 attach-reply   (qid qid))

;; Rerror ename[s]
(define-message 107 error-reply (ename name))

;; Tflush oldtag[2]
;; Rflush
(define-message 108 flush-request (oldtag tag))
(define-message 109 flush-reply)


;; Twalk fid[4] newfid[4] nwname[2] nwname*(wname[s])
;; Rwalk nwqid[2] nwqid*(wqid[13])
(define-message 110 walk-request (fid fid) (newfid fid) (names names))
(define-message 111 walk-reply (qids qids))

;; Topen fid[4] mode[1]
;; Ropen qid[13] iounit[4]
(define-message 112 open-request (fid fid) (mode mode))
(define-message 113 open-reply (qid qid) (iounit size))

;; Tcreate fid[4] name[s] perm[4] mode[1]
;; Rcreate qid[13] iounit[4]
(define-message 114 create-request (fid fid) (name name) (perm perm) (mode mode))
(define-message 115 create-reply   (qid qid) (iounit size))

;; Tread fid[4] offset[8] count[4]
;; Rread count[4] data[count]
(define-message 116 read-request (fid fid) (offset offset) (count size))
(define-message 117 read-reply   (data data))

;; Twrite fid[4] offset[8] count[4] data[count]
;; Rwrite count[4]
(define-message 118 write-request (fid fid) (offset offset) (data data))
(define-message 119 write-reply   (count size))

;; Tclunk fid[4]
;; Rclunk
(define-message 120 clunk-request (fid fid))
(define-message 121 clunk-reply)

;; Tremove fid[4]
;; Rremove
(define-message 122 remove-request (fid fid))
(define-message 123 remove-reply)

;; Tstat fid[4]
;; Rstat stat[n]
(define-message 124 stat-request (fid fid))
(define-message 125 stat-reply   (stat stat))

;; Twstat fid[4] stat[n]
;; Rwstat
(define-message 126 wstat-request (fid fid) (stat stat))
(define-message 127 wstat-reply)


;;; Usage:
#|(setf *message-size* 128)

(let* ((message (with-output-to-sequence (stream)
                  (write-walk-reply #xffff '(#x04 #x05) stream))))
  (multiple-value-bind (type buffer)
      (with-input-from-sequence (stream message) (read-message stream))
    (print type)
    (read-walk-reply buffer)))
|#


;;; Imaginary client interface:
;;; (with-session (root &key uname aname auth-fnmsize)
;;;   (walk (root "hello")
;;;     ;; On success.
;;;     ((newfid) &body)
;;;     ;; On failure.
;;;     ((ename) &body)))


;;; Imaginary server interface:
;;; (make-filesysem aname
;;;                 :walk (function (fid name) (values qids))
;;;                 ...)
