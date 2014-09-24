;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(asdf:defsystem :drakma-spider
    :description "a simple wrapper of dramka"
    :version "0.1"
    :author "snowh4r3 O <snowh4r3@gmail.com>"
    :license "BSD 2-Clause License"
    :depends-on ("drakma"
                 "st-json"
                 "uffi"
                 "cl-smtp"
                 "cl-pop"
                 "babel"
                 "cl-ppcre"
                 "closure-html"
                 "babel"
                 "cxml-stp"
                 "split-sequence"
                 "trivial-dump-core"
                 )
    :components ((:file "package")
                 (:file "user_agents" :depends-on ("package"))
                 (:file "common" :depends-on ("package"))
                 (:file "test" :depends-on ("package"))
                 (:file "drakma" :depends-on ("package" "common"))
                 (:file "email" :depends-on ("package"))
                 ))

