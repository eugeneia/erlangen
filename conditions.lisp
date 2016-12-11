;;;; Generic conditions.

(in-package :erlangen.conditions)

(define-condition timeout (error) ()
  (:documentation
   "*Description:*

    Describes an error condition that can occur when using functions with
    a timeout. It denotes a that the operation was unable to
    successfully complete within a given duration."))
