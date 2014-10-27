(in-package :nixiwan)

(define-condition failed-to-get-content (error)
  ((message :initform nil :initarg :message :reader get-content-failed-message)))
