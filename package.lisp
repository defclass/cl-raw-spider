(defpackage :snowh4r3-common
  (:use :cl
        :cl-ppcre
        :curl
        :cl-smtp
        :cl-pop
        :babel
        :cl-ppcre
        :closure-html
        :babel)
  (:export :test-curl))

(defpackage :snowh4r3-spider
  (:use :cl
        :cl-ppcre
        :curl
        :cl-smtp
        :cl-pop
        :babel
        :cl-ppcre
        :snowh4r3-common
        :closure-html
        :babel)
  (:export :test-curl))

(defpackage :snowh4r3-test
  (:use :cl
        :cl-ppcre
        :curl
        :cl-smtp
        :cl-pop
        :babel
        :cl-ppcre
        :closure-html
        :babel)
  (:export :test-curl))

(defpackage :snowh4r3-email
  (:use :cl
        :cl-ppcre
        :curl
        :cl-smtp
        :cl-pop
        :babel
        :cl-ppcre
        :closure-html
        :babel)
  (:export :test-curl))
