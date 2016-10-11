;;;; Generic conditions.

(in-package :erlangen.conditions)

(define-condition send-error (error) ()
  (:documentation
   "*Description:*

    Describes an error condition that can occur when calling {send}. It
    denotes a that {send} was unable to successfully deliver the message
    to the recepient."))

(define-condition timeout (error) ()
  (:documentation
   "*Description:*

    Describes an error condition that can occur when using functions with
    a timeout. It denotes a that the operations was unable to
    successfully complete within a given deadline."))
