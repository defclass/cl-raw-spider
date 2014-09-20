;;;(asdf-install:install "./libs/cl-curl.tgz")
(cl:in-package :snowh4r3-spider)

(defparameter origin-url '(("prefix" . "http://www.alibaba.com/products/F0/plastic_recycle/----------------------50----------------------------EU")
                           ("suffix" . ".html")))
(defparameter total-index-page 2)

(defun get-index-urls ()
  " Get alibaba index page url"
  (let* ((max-page total-index-page)
         (pre-url (cdr (assoc "prefix" origin-url :test  #'string=)))
         (suffix-url (cdr (assoc "suffix" origin-url  :test #'string=))))
    (loop for i from 0 to max-page
       collect
         (cond
           ((= 0 i) (concatenate 'string pre-url suffix-url))
           (t (concatenate 'string  pre-url (write-to-string i) suffix-url))))))


(defun get-content (url &key (encode :utf8))
  " Get the webpage content"
  (progn
    (snowh4r3-common::write-log (concatenate 'string "正在获取网址:" url))
    (snowh4r3-common::write-log (drakma:http-request url :external-format-in encode) :stdout nil)))

(defun parse-contact-url (str)
  " get alibaba contact info pages .Return urls"
  (let* ((document (chtml:parse str (cxml-stp:make-builder)))
         (collect '()))
    (stp:do-recursively (a document)
      (when (and (typep a 'stp:element)
                 (equal (stp:local-name a) "a")
                 (equal (stp:attribute-value a "class") "cd dot-product"))
        (let ((url (stp:attribute-value a "href")))
          (when (not (find url collect :test #'string=))
              (setf collect (append (list url) collect))))))
              collect))

(defparameter test-url "http://herbold.trustpass.alibaba.com/contactinfo.html")

(defun parse-contact-info ()
  " Get alibaba contact info . Include
    Telephone:
    Fax,
    Address,
    Zip,
    Country/Region,City,
 	Company Name: 	
	Operational Address: 	
	Website:
	Website on alibaba.com.Return a alist "
  
  (let* ((str (snowh4r3-common::read-file-to-str "~/Desktop/lisp.html"))
         (document (chtml:parse str (cxml-stp:make-builder)))
         (collect ()))
    (stp:do-recursively (a document)
      (when (and (typep a 'stp:element)
                 (equal (stp:local-name a) "div")
                 (equal (stp:attribute-value a "class") "contact-detail"))
        (let*  ((body (stp:nth-child 1 a))
                (father-node (stp:list-children body))
                (raw-list 
                 (loop for i in father-node 
                    if  (oddp (position i father-node ))
                    collecting (stp:data (stp:first-child i))  into odds
                    else
                    collecting (stp:data (stp:first-child i))  into evens
                    finally (return (list evens odds)))))
          (destructuring-bind (x y) raw-list
            (setf collect (pairlis x y collect)))))
      
      (when (and (typep a 'stp:element)
                 (equal (stp:local-name a) "table")
                 (equal (stp:attribute-value a "class") "company-info-data table"))
        (let* ((body (stp:first-child a))
               (last-child (stp:last-child body))
               (th '())
               (td '()))
          (progn
            (stp:delete-child last-child body)
            (stp:do-recursively (c body )
              ;;; select th tag
              (when (and (typep c 'stp:element)
                         (equal (stp:local-name c) "th"))
                (push  (stp:data (stp:first-child c)) th))
              ;;; select td tag
              (when (and (typep c 'stp:element)
                         (equal (stp:local-name c) "td")
                         (not (equal (stp:attribute-value c "class") "icon-col")))
                (let* ((predicate-num (stp:count-children  "a" c
                                                      :test #'(lambda (x y)
                                                                (and (typep y 'stp:element)
                                                                     (equal (stp:local-name y) x))))))
                  (if (> predicate-num 1)
                      (let* ((result '()))
                        (stp:do-recursively (d c)
                          (when (and (typep d 'stp:text)
                                     (< 4 (length (string-trim '(#\Space #\Tab #\Return #\Newline) (stp:data d)))))
                            (push (stp:data d) result)))
                        (push result td))
                      (stp:do-recursively (d c)
                        (when (and (typep d 'stp:text)
                                   (< 4 (length (string-trim '(#\Space #\Tab #\Return #\Newline) (stp:data d)))))
                          (push (stp:data d) td))))))))
          (setf collect (pairlis th td collect)))))
    collect))
              
