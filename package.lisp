(defpackage :common
  (:use :cl :cl-ppcre :cl-smtp :cl-pop)
  (:export
   ;;;;;;;;;;;;;;;;;;;
   ;;;; functions
   ;;;;;;;;;;;;;;;;;;;
   plist-value
   write-log
   read-file-to-str
   ;;;;;;;;;;;;;;;;;;;;
   ;;;; variable
   ;;;;;;;;;;;;;;;;;;;;
   user-agents
   ))


(defpackage :config
  (:use :cl :common)
  (:export c))

(defpackage :daogou
  (:use :cl :common :config)
  (:export *recoder-hash* *all-contacts-info*
           *contacts-and-emails* *recorder-hash*))


(defpackage :test
  (:use :cl :cl-ppcre :common :cl-smtp :cl-pop))

