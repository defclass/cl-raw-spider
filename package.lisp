(defpackage :snowh4r3-common
  (:use
   :cl
   :cl-ppcre
   :cl-smtp
   :cl-pop)
  (:export
   ;;;;;;;;;;;;;;;;;;;
   ;;;; functions
   ;;;;;;;;;;;;;;;;;;;
   plist-value
   write-log
   read-file-to-str
   ))

(defpackage :snowh4r3-spider
  (:use
   :cl
   :snowh4r3-common
   :cl-ppcre
   :cl-smtp
   :cl-pop
   :babel)
  (:export
   *recoder-hash*
   *all-contacts-info*    
   *contacts-and-emails*  
   *recorder-hash*))

(defpackage :snowh4r3-test
  (:use :cl
        :cl-ppcre
        :snowh4r3-common
        :cl-smtp
        :cl-pop))

(defpackage :snowh4r3-email
  (:use :cl
        :cl-ppcre
        :snowh4r3-common
        :cl-smtp
        :cl-pop))
