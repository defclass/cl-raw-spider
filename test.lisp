(cl:in-package :test)

(defun test-cxml ()
  (let* ((str (drakma:http-request "http://www.baidu.com/"))
         (document (chtml:parse str (cxml-stp:make-builder))))
    (stp:do-recursively (a document)
      
      (when (and (typep a 'stp:element)
                 (equal (stp:local-name a) "a")
                 (equal (stp:attribute-value a "class") "mnav")
                 (equal (stp:attribute-value a "name") "tj_trnews"))
      (format t (stp:attribute-value a "href"))))))



(defun test-bable ()
  (let* ((r (drakma:http-request "http://www.sina.com.cn/" :force-binary t)))
         (babel:octets-to-string r :encoding :GB2312)))




(defun test-drakma (url)
      (drakma:http-request url :external-format-in ) )

;;;(defparameter conn (cl-pop::open-pop-connection :host "pop.qq.com" :username "snowh4r3@qq.com" :password "~hQ19880607")
(defun test-socket ()
;;;  (let* ((socket (usocket::socket-connect "pop.qq.com" 110 :element-type '(unsigned-byte 8)))
  (let* ((socket (usocket::socket-connect "pop.qq.com" 110 ))
         (s (usocket:socket-stream socket)))
    (setf s (flexi-streams::make-flexi-stream s :external-format :gbk))
    (format t "~a~%" (read-line s))))

(defun test-read-line-gbk ()
  (let* ((path "/tmp/slim.txt"))
    (with-open-file (s path :element-type '(unsigned-byte 8))
      (let ((gbk-stream (flexi-streams::make-flexi-stream s :external-format :gbk)))
        (format t "~A~%" (read-line gbk-stream))))))

(defun show-google-hits (term)
  (let* ((query (list (cons "q" term)))
         (str (drakma:http-request "http://www.google.com/search"
                                   :parameters query))
         (document (chtml:parse str (cxml-stp:make-builder))))
    (stp:do-recursively (a document)
      (when (and (typep a 'stp:element)
                 (equal (stp:local-name a) "a")
                 (equal (stp:parent a) "h3"))
        (format t "~A:~%  " (stp:string-value a))))))

(defun test-list ()
  (let* ((list '(1 2 3 4 5 6)))
    (loop for i in list
       if  (oddp (position i list ))
       collecting i into odds
       else
       collecting i into evens
       finally (return (list evens odds)))))

(defun test-ppcre ()
  (let* ((var "lsdkfdl sbcdsd23@sld.comd lsldlfkkl"))
    (cl-ppcre:all-matches-as-strings "\w" var)))

(defun test-tagbody()
  (labels ((abc (x)
             (+ x 5)))
    (abc 1)))

(defun test-condition ()
  (format t "~A" (test-tagbody)))

(defmacro test-macro ()
  (PROGN
   `(format t "~A" '((+ 5 6)  b))
   (format t "~A" '((+ 5 7) b))))


(defun test-teturn-nil(x)
  (when (not x )
    (format t "abc")))


(defun test-shell()
  (sb-ext:run-program "/usr/bin/sh" (list "-c" "curl --silent --write-out %{http_code}  http://www.sina.com.cn") ))

(define-condition test-condetion ()
  ((code :initarg :text :reader read-code)))

(defun test-sb-ext (&key (code :utf8) )
  (let* ((r (drakma:http-request "http://www.sina.com.cn" :force-binary t)))
    (sb-ext:octets-to-string r :EXTERNAL-FORMAT code )))


(defun  do-request ()
  (handler-case (test-sb-ext)
    (SB-IMPL::INVALID-UTF8-CONTINUATION-BYTE () nil)))
      
(defun hello-world()
  (format t "Hello world"))


(defclass test ()
  ((name :initarg :name :accessor name)
   (age :initarg :age :accessor age)))
