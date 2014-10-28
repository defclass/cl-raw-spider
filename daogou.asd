;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(asdf:defsystem :daogou
    :version "0.1"
    :serial t
    :author "snowh4r3 O <snowh4r3@gmail.com>"
    :depends-on (:drakma
                 :cl-ppcre
                 :cl-json
                 :crane
                 :alexandria
                 :st-json
                 :local-time
                 :closer-mop
                 :symbol-munger
                 :iterate
                 :log4cl
                 :cl-store)
    :description "nixiwan.com"
    :components ((:file "packages")
                 (:file "user_agents")
                 (:file "specials")
                 (:file "config")
                 (:file "util")
                 (:file "conditions")
                 (:file "daogou" )))

