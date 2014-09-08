(asdf:defsystem :drakma-spider
    :description "a simple wrapper of dramka"
    :version "0.1"
    :author "snowh4r3 O <snowh4r3@gmail.com>"
    :license "BSD 2-Clause License"
    :depends-on ("drakma"
                 "st-json"
                 "cl-ppcre"
                 "closure-html"
                 "babel"
                 "cxml-stp"
                 "split-sequence")
    :components ((:file "drakma")
                 ))

