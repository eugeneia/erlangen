;;;; Generic logging agent.

(defpackage erlangen-platform.log
  (:use :cl :erlangen)
  (:export :*log*
           :logger
           :write-log
           :write-log*
           :make-timestamp
           :to-standard-output))

(in-package :erlangen-platform.log)

(defvar *log* :log
  "*Value Type:*

   an _agent_.

   *Description:*

   This _variable_ can be _bound_ or _assigned_ in order to change the default
   destination for log entries written by {write-log}.")

(let ((standard-output *standard-output*))
  (defun to-standard-output (message)
    "*Arguments and Values:*

     _message_—an _object_.

     *Description*:

     {to-standard-output} prints _message_ to {*standard-output*}"
    (print message standard-output)))

(defun logger (&key (name :log) (log-function 'to-standard-output))
  "*Arguments and Values:*

   _name_—a _keyword_. Default is {:log}.

   _log-function_—a unary _function designator_. Default is
   {to-standard-output}.

   *Description*:

   {logger} registers itself for _name_, and calls _log-function_ on messages
   it receives."
  (register name)
  (unwind-protect (loop for message = (receive)
                     do (ignore-errors (funcall log-function message)))
    (unregister name)))

(defun make-timestamp (&optional (universal-time (get-universal-time)))
  "*Arguments and Values:*

   _universal-time_—a _universal time_. Default is the current _universal
   time_ as returned by {get-universal-time}.

   *Description*:

   {make-timestamp} returns a _timestamp_ that represents
   _universal-time_. _Timestamp_ is a _string_ of the following form:

   _YYYY-MM-DD hh:mm:ss_"
  (multiple-value-bind (second minute hour date month year
                               ; We don't care.
                               day daylight-p zone)
      (decode-universal-time universal-time 0)
    (declare (ignore day daylight-p zone))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(defun write-log (message &optional (log *log*))
  "*Arguments and Values:*

   _message_—an _object_.

   _log_—an _agent_. Default is the _value_ of {*log*}.

   *Description*:

   {write-log} tags _message_ with the _calling agent_ and a _timestamp_
   and sends it to _log_."
  (send (list* (agent) (make-timestamp) message) log))

(defmacro write-log* (message &optional (log *log*)
                      &aux (log-sym (gensym "log")))
  "*Arguments and Values:*

   _message_—an _object_.

   _log_—an _agent_ or {nil}. Default is the _value_ of {*log*}.

   *Description*:

   If _log_ is a _non-nil object_, {write-log*} tags _message_ with the
   _calling agent_ and a _timestamp_ and sends it to _log_.

   {write-log*} has no effect if _log_ is {nil}."
  `(let ((,log-sym ,log))
     (when ,log-sym
       (write-log ,message ,log-sym))))
