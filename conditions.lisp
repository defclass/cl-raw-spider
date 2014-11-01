(in-package :nixiwan)

(define-condition failed-to-get-content (error)
  ((message :initform nil :initarg :message :reader get-content-failed-message)))

(define-condition decode-mall-url-error (error)
  ((message :initform nil :initarg :message :reader ecode-mall-url-error)))

(define-condition replace-promote-empty-url (error)
  ((message :initform nil :initarg :message :reader replace-promote-empty-url)))

(define-condition json-error (error)
  ((message :initform nil :initarg :message :reader get-json-error)))

(define-condition mysql-string-empty (error)
  ((message :initform nil :initarg :message :reader get-empty-string)))

(define-condition find-dividing-arg-error (error)
  ((message :initform nil :initarg :message :reader find-dividing-arg)))

(define-condition collect-goods-arg-error (error)
  ((message :initform nil :initarg :message :reader collect-goods-arg)))





