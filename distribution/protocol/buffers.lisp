;;;; Protocol wire format DSL. Core is the DEFINE-MESSAGE macro:
;;;;
;;;; define-message <mtype> <mname> [<field>]*
;;;;
;;;;   field::= (<fname> <ftype>)
;;;;   ftype::= integer|string|value
;;;;
;;;; Where MNAME and FNAME are symbols and MTYPE is an unsigned
;;;; integer. The FTYPEs are as follows:
;;;;
;;;;   integer - unsigned-byte 32
;;;;   string - a string
;;;;   value - any value encodable by cl-conspack
;;;;
;;;; DEFINE-MESSAGE will define the functions
;;;;
;;;;   write-<mname> [field-value]* &optional stream
;;;;   read-<mname> octets => [field-value]*
;;;;
;;;; respectively. WRITE-<MNAME> writes a message of type <MTYPE> with
;;;; [FIELD-VALUE]* to STREAM (if supplied) or returns it as a vector of
;;;; octets. READ-<MNAME> reads a message of type <MTYPE> from OCTETS and
;;;; return [FIELD-VALUE]*. Use these with READ-MESSAGE and
;;;; READ-MESSAGE*.

(in-package :erlangen.distribution.protocol.buffers)

(defun writer-symbol (symbol)
  (intern (format nil "WRITE-~a" (symbol-name symbol))))

(defun reader-symbol (symbol)
  (intern (format nil "READ-~a" (symbol-name symbol))))

;;;; Supported types:
;;;;  * integer (unsigned-byte 32, little endian)
;;;;  * string (a UTF-8 encoded octet vector lead by an `integer'
;;;;    specifying its length)
;;;;  * value (a conspack encoded value)

(defun compile-string-to-octets (name)
  `(string-to-utf-8-bytes ,name))

(defun compile-encode-value (name)
  `(encode ,name))

(defconstant integer-size 4)
(defun compile-compute-size (fields)
  `(+ ,@(loop for (name type) in fields collect
             (ecase type
               (integer integer-size)
               ((string value) `(+ integer-size (length ,name)))))))

(defun check-integer (name integer)
  (unless (typep integer '(unsigned-byte 32))
    (error 'type-error
           :format-control "~a does not fit type ~a."
           :datum name
           :expected-type '(unsigned-byte 32))))

(defun compile-write-integer (name)
  `(progn
     (check-integer ',name ,name)
     (writeu32-le ,name buffer)))

(defun compile-write-octets (name)
  `(progn
     (check-integer ,(format nil "Length of ~a" name) (length ,name))
     (writeu32-le (length ,name) buffer)
     (fast-write-sequence ,name buffer)))

(defun compile-write-message (type fields)
  `(with-fast-output (buffer (or stream :vector))
     (writeu32-le ,type buffer)
     (let ,(loop for (name type) in fields collect
                `(,name ,(ecase type
                           (integer name)
                           (string (compile-string-to-octets name))
                           (value (compile-encode-value name)))))
       (let ((message-size ,(compile-compute-size fields)))
         (check-integer 'message-size message-size)
         (writeu32-le message-size buffer))
       ,@(loop for (name type) in fields collect
              (ecase type
                (integer        (compile-write-integer name))
                ((string value) (compile-write-octets name)))))))

(defun compile-read-octets (decode-function)
  `(let* ((length (readu32-le buffer))
          (octets (make-octet-vector length)))
     (unless (= (fast-read-sequence octets buffer) length)
       (error 'end-of-file :stream buffer))
     (funcall ',decode-function octets)))

(defun decode-interning (octets)
  "See https://github.com/conspack/cl-conspack/issues/1"
  (with-interning () (decode octets)))

(defun compile-read-message (fields)
  `(with-fast-input (buffer octets)
     (values
      ,@(loop for (name type) in fields collect
             (ecase type
               (integer '(readu32-le buffer))
               (string (compile-read-octets 'utf-8-bytes-to-string))
               (value (compile-read-octets 'decode-interning)))))))

(defmacro define-message (type name &rest fields)
  "Define reader and writer functions for message of TYPE with FIELDS
 (see package documentation). NAME will be the suffix of the defined
 functions name."
  `(progn
     (defun ,(writer-symbol name) (,@(mapcar 'first fields)
                                   &optional stream)
       ,(compile-write-message type fields))
     (defun ,(reader-symbol name) (octets)
       ,(compile-read-message fields))
     (values)))

(defun read-message (stream)
  "Read a message from STREAM and return its type and the message as an
octet vector."
  (with-fast-input (buffer nil stream)
    (values (readu32-le buffer)
            (let* ((length (readu32-le buffer))
                   (message (make-octet-vector length)))
              (unless (= (fast-read-sequence message buffer) length)
                (error 'end-of-file :stream stream))
              message))))

(defun read-message* (type reader stream)
  "Read a message of TYPE from STREAM using READER."
  (with-fast-input (buffer nil stream)
    (unless (= (readu32-le buffer) type)
      (error "Invalid message type."))
    (let* ((length (readu32-le buffer))
           (message (make-octet-vector length)))
      (unless (= (fast-read-sequence message buffer) length)
        (error 'end-of-file :stream stream))
      (funcall reader message))))
