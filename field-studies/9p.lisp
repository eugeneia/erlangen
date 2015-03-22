;;;; Implementation of the 9P2000 protocol.

(defpackage nodes.9p ;; Aka `neun', spin off as library.
  (:use :cl :bytes :flexi-streams))

(in-package :nodes.9p)


(defun writer-symbol (symbol)
  (intern (format nil "WRITE-~a" (symbol-name symbol)) :nodes.9p))

(defun reader-symbol (symbol)
  (intern (format nil "READ-~a" (symbol-name symbol)) :nodes.9p))

(defun compile-write-integer (size)
  `(write-bytes datum ,size stream))

(defun compile-9p-string-writer (bytes)
  `(progn
     ;; size[bytes]
     (write-bytes (length datum) ,bytes stream)
     ;; string
     (write-sequence datum stream)))

(defun compile-write-vector (element-type)
  `(progn
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
     (unless (> (read-sequence buffer stream) size)
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
          (integer (compile-write-integer    spec))
          (string  (compile-9p-string-writer spec))
          (vector  (compile-write-vector     spec)))
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


(defparameter *message-types*
  '((version-request . 100)
    (version-reply   . 101)
    (auth-request    . 102)
    (auth-reply      . 103)
    (attach-request  . 104)
    (attach-reply    . 105)
    (error-reply     . 107)
    (fluch-request   . 108)
    (flush-reply     . 109)
    (walk-request    . 110)
    (walk-reply      . 111)
    (open-request    . 112)
    (open-reply      . 113)
    (create-request  . 114)
    (create-reply    . 115)
    (read-request    . 116)
    (read-reply      . 117)
    (write-request   . 118)
    (write-reply     . 119)
    (clunk-request   . 120)
    (clunk-reply     . 121)
    (remove-request  . 122)
    (remove-reply    . 123)
    (stat-request    . 124)
    (stat-reply      . 125)
    (wstat-request   . 126)
    (wstat-reply     . 127))
  "Message symbol to integer mapping.")

(defparameter *message-table*
  (make-hash-table :test 'eql :size (length *message-types*))
  "Lookup table for integer=>symbol.")

(defvar *message-size* nil
  "Set to `msize' during `session'.")

(defun message-type (name)
  (get name :message-type))

(defun compile-compute-9p-string-size (name type)
  `(+ ,(get type :datum-spec) (length ,name)))

(defun compile-compute-vector-size (name type)
  `(+ 2 ; `length'
      ,(let ((subdatum (get type :datum-spec)))
            (ecase (get subdatum :datum-type)
              (integer `(* ,(get subdatum :datum-spec) (length ,name)))
              (string  `(+ ,(get subdatum :datum-spec)
                           (loop for element in ,name sum
                                ,(compile-compute-9p-string-size
                                  'element subdatum))))))))

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
       (gethash (read-type stream) *message-table*)
       (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
         (unless (> (read-sequence buffer stream) (- size header-size))
           (error 'end-of-file :stream stream))
         buffer)))))

(defun compile-read-message (name fields)
  `(values (read-tag stream)
           ,@(loop for (name type) in fields collect
                  `(,(reader-symbol type) stream))))

(defmacro define-message (name &rest fields)
  `(progn
     (defun ,(writer-symbol name) (tag ,@(mapcar #'car fields) stream)
       ,(compile-write-message name fields))
     (defun ,(reader-symbol name) (stream)
       ,(compile-read-message name fields))
     (values)))


;; Define messages.
(eval-when (:compile-toplevel)
  (loop for (name . type) in *message-types*
     do (setf (get name :message-type) type
              (gethash type *message-table*) name)))

;; Tversion msize[4] version[s]
;; Rversion msize[4] version[s]
(define-message version-request (msize size) (version name))
(define-message version-reply   (msize size) (version name))

;; Tauth afid[4] uname[s] aname[s]
;; Rauth aqid[13]
(define-message auth-request (afid fid) (uname name) (aname name))
(define-message auth-reply   (aqid qid))

;; Rerror ename[s]
(define-message error-reply (ename name))

;; Tflush oldtag[2]
;; Rflush
(define-message flush-request (oldtag tag))
(define-message flush-reply)

;; Tattach fid[4] afid[4] uname[s] aname[s]
;; Rattach qid[13]
(define-message attach-request (fid fid) (afid fid) (uname name) (aname name))
(define-message attach-reply   (qid qid))

;; Twalk fid[4] newfid[4] nwname[2] nwname*(wname[s])
;; Rwalk nwqid[2] nwqid*(wqid[13])
(define-message walk-request (fid fid) (newfid fid) (names names))
(define-message walk-request (qids qids))

;; Topen fid[4] mode[1]
;; Ropen qid[13] iounit[4]
(define-message open-request (fid fid) (mode mode))
(define-message open-request (qid qid) (iounit size))

;; Tcreate fid[4] name[s] perm[4] mode[1]
;; Rcreate qid[13] iounit[4]
(define-message create-request (fid fid) (name name) (perm perm) (mode mode))
(define-message create-reply   (qid qid) (iounit size))

;; Tread fid[4] offset[8] count[4]
;; Rread count[4] data[count]
(define-message read-request (fid fid) (offset offset) (count size))
(define-message read-reply   (data data))

;; Twrite fid[4] offset[8] count[4] data[count]
;; Rwrite count[4]
(define-message write-request (fid fid) (offset offset) (data data))
(define-message write-reply   (count size))

;; Tclunk fid[4]
;; Rclunk
(define-message clunk-request (fid fid))
(define-message clunk-reply)

;; Tremove fid[4]
;; Rremove
(define-message remove-request (fid fid))
(define-message remove-reply)

;; Tstat fid[4]
;; Rstat stat[n]
(define-message stat-request (fid fid))
(define-message stat-reply   (stat stat))

;; Twstat fid[4] stat[n]
;; Rwstat
(define-message wstat-request (fid fid) (stat stat))
(define-message wstat-reply)


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
