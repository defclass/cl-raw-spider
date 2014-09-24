;;;(asdf-install:install "./libs/cl-curl.tgz")
(cl:in-package :snowh4r3-spider)
(defparameter origin-url '(("prefix" . "http://www.alibaba.com/products/F0/plastic_recycle/----------------------20----------------------------SEA")
                           ("suffix" . ".html")))
(defparameter total-index-page 0)
(defvar output-file "~/Desktop/out-put-file.cvs")


(defvar *contacts-url* nil)
(defvar *all-contacts-info* nil)
(defvar *contacts-and-emails* nil)
(defvar *recorder-hash* (make-hash-table))




(defun do-crawl ()
  " main function"
  )

  ;; (let* ((alibaba-index (get-index-urls)))
  ;;   (progn
  ;;     (setf *contacts-url* (do-get-and-parse alibaba-index parse-contact-url append))
  ;;     (setf *contacts-url* (remove nil *contacts-url*))
  ;;     (setf *all-contacts-info* (do-get-and-parse  *contacts-url* parse-contact-info collect))
  ;;     (set *contacts-and-emails* (loop for i in *all-contacts-info*
  ;;                                   collect (concatenate 'list (cons "email" emails) i)))
  ;;     (set *recorder-hash* (loop for i in *all-contacts-info*
  ;;                             do (setf (gethash (cdr (assoc "Company Name:" i)) *recorder-hash*) i))))))

          ;; (handler-case (write-data-to-file *recorder-hash*)
          ;;   (t () (format t "~A~%" *recorder-hash*)))))))))



;; (defun do-get-emails (list)
;;   (let*  ((gg-url nil)
;;           (gg-content nil)
;;           (urls-in-gg nil)
;;           (emails nil))
;;     (progn
;;       (setf emails (if (and (setf gg-url (assemble-google-url list))
;;                             (setf gg-content (get-content gg-url))
;;                             (setf urls-in-gg (parse-gg-index-url gg-content))
;;                             (typep urls-in-gg 'list))
;;                        (do-get-and-parse urls-in-gg seek-email-in-str append)
;;                        nil))
;;       (if emails
;;           (score-and-sort-emails (cdr (assoc "Website:" list)) emails)
;;           nil))))


(defmacro get-next-value ((save-var save-keyname) (base-var base-keyname)  handle-func)
  (let*  ((var (gensym)))
  `(setf ,save-var
         (loop for i in ,base-var with ,var = nil
            when (setf ,var  (let* ((base-value (snowh4r3-common:plist-value ,base-keyname i)))
                                           (if base-value
                                               (funcall ,handle-func base-value)
                                               nil)))
            collect (acons ,save-keyname ,var i)))))

(defvar *contacts-and-gg-url*
  " add a field to save the google search url ,eg:
http://www.gooogle.com.hk/?q=xxx"  nil)

(defvar *contacts-and-gg-content*
  " contacts and google search page 's content string"
  nil)

(defvar *contacts-and-gg-index-url*
  " add a field to save the result url that google have searched"
  nil)


(get-next-value (*contacts-and-gg-url* "gg-url")
                (*all-contacts-info* "Website:")
                #'assemble-google-url)

(get-next-value (*contacts-and-gg-content* "gg-content")
                (*contacts-and-gg-url* "gg-url")
                #'get-content)

(get-next-value (*contacts-and-gg-index-url* "gg-index-urls")
                (*contacts-and-gg-content* "gg-content")
                #'parse-gg-index-url)


(defun get-ali-contact-info ()
  (let*((alibaba-index (get-ali-index-urls)))
    (progn
      (setf *contacts-url* (do-get-and-parse alibaba-index #'parse-contact-url append))
      (setf *contacts-url* (remove nil *contacts-url*))
      (setf *all-contacts-info* (do-get-and-parse  *contacts-url* #'parse-contact-info collect)))))
         
       

(defmacro do-get-and-parse (urls parse-func collect-keyword)
  `(loop for a in ,urls
        ,collect-keyword (let* ((content (get-content a)))
                           (if content
                               (funcall ,parse-func content)
                               nil))))

(defun get-ali-index-urls ()
  " Get alibaba index page url"
  (let* ((max-page total-index-page)
         (pre-url (cdr (assoc "prefix" origin-url :test  #'string=)))
         (suffix-url (cdr (assoc "suffix" origin-url  :test #'string=))))
    (loop for i from 0 to max-page
       collect
         (cond
           ((= 0 i) (concatenate 'string pre-url suffix-url))
           (t (concatenate 'string  pre-url "/" (write-to-string i)  suffix-url))))))


(defun get-content (url &key (encode :utf8))
  " Get the webpage content"
  (if   (cl-ppcre:all-matches-as-strings "^https?:/{2}\\w.+$" url)
        
        (let* ((user-agent (elt user-agents (random (length user-agents)))))
          (progn
            ;; (when (search "google" url :test #'equal)
            ;;   (progn
            ;;     (snowh4r3-common::write-log (concatenate 'string "google网址休息一下"))))
          ;;;(sleep (+ 5 (random 5)))))
            (snowh4r3-common::write-log (concatenate 'string "正在获取网址:" url ))
            (handler-case
                (multiple-value-bind (content status-code)
                    (drakma:http-request url :external-format-in encode   :user-agent user-agent )
                  (if (eql status-code 200)
                      (progn
                        (snowh4r3-common::write-log content :stdout nil)
                        content)
                      nil))
        ;;;;(FLEXI-STREAMS:EXTERNAL-FORMAT-ENCODING-ERROR ()  (format t  "编码不对无法解析\n"))
              (FLEXI-STREAMS:EXTERNAL-FORMAT-ENCODING-ERROR ()
                (progn
                  (format t "str解码错误~%")
                  nil))
              (USOCKET:TIMEOUT-ERROR ()
                (progn
                  (format t "url超时错误%")
                  nil))

              )))
        nil))

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

(defun parse-contact-info (str)
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
  (let* ((document (chtml:parse str (cxml-stp:make-builder)))
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
                      (let* ((result ()))
                        (stp:do-recursively (d c)
                          (when (and (typep d 'stp:text)
                                     (< 4 (length (string-trim '(#\Space #\Tab #\Return #\Newline) (stp:data d)))))
                            (push (stp:data d) result)))
                        (push result td))
                      (stp:do-recursively (d c)
                        (when (and (typep d 'stp:text)
                                   (< 4 (length (string-trim '(#\Space #\Tab #\Return #\Newline) (stp:data d)))))
                          (push (stp:data d) td)))))))
            (setf collect (pairlis th td collect))))))
  collect))
              

(defun seek-email-in-str (str)
  (cl-ppcre:all-matches-as-strings "\\w+@[a-zA-Z0-9-_]+\\.\\w+" str))

(defun assemble-google-url (site-filed)
  (let* ((base-url "https://www.google.com.hk/search?hl=en&q=")
         (site-url (labels ((remove-http (url)
                              " remove http:// prefix "
                              (string-trim "http://" url)))
                     (if (and site-filed
                              (typep site-filed 'list)
                              (< 0 (length site-filed)))
                         (reduce #'(lambda (x y) (CONCATENATE 'string (remove-http x) "+" (remove-http y))) site-filed)
                         (remove-http site-filed)))))
    (cond (site-url (concatenate 'string base-url  site-url  "+email"))
          (t nil))))

(defun parse-gg-index-url (str)
  " 获取 google 搜索页面的 url地址 "
  (let* ((document (chtml:parse str (cxml-stp:make-builder)))
         (links (stp:filter-recursively #'(lambda (y)
                                            (and (typep y 'stp:element)
                                                 (equal (stp:local-name y ) "h3")
                                                 (equal (stp:attribute-value y "class" ) "r"))) document)))
         (map 'list  #'(lambda (x)
                         (string-trim "/url?q=" (stp:attribute-value (stp:first-child x) "href"))) links)))

(defun score-and-sort-emails(url emails)
  " url 可能是list ,email一定是list ,对email进行打分后排序，取前面两个email"
  (flet ((get-domain (url)
           (cond
             ((typep url 'list)  (map 'list #'(lambda(x)
                                                (string-trim "www." (string-trim "http://" x))) url))
             ((equal "" (string-trim '(#\Space #\Tab #\Return #\Newline) url)) nil)
             (t (string-trim "www." (string-trim "http://" url))))))
    (let* ((domain (get-domain url))
           scored-email sort-email)
      (when domain
        (flet ((get-score (email)
                 (let* ((splited-email (cl-ppcre:split "@" email))
                        (email-prefix (list "info" "webmaster" "trade" "sales"))
                        (score 0))
                   (progn
                     (when (find (car splited-email) email-prefix :test #'equalp)
                       (setf score (+ 1 score)))
                     (when (or (and (typep domain 'list)
                                    (find (cadr splited-email) domain :test #'equalp))
                               (typep domain 'string))
                       (setf score (+ 10 score))))
                   score)))
          (progn
            (setf scored-email (map 'list #'(lambda(email)
                                              (cons email (get-score email))) emails))
            (setf sort-email (sort scored-email #'(lambda (x y)
                                                    (> (cdr x)
                                                       (cdr y)))))
            (subseq (map 'list #'(lambda (email)
                                   (car email)) sort-email) 0  2)))))))

(defun trunc-a-recorder (a-recorder)
  (let* ((index '("Telephone:"
                 "Fax:"
                 "Address:"
                 "Zip:"
                 "Country/Region:"
                 "City:"
                 "Company Name:"
                 "Operational Address:"
                 "Website:"
                 "Website on alibaba.com:")))
         (map 'list  #'(lambda (x)
                         (if (assoc x a-recorder  :test #'equal)
                             (cons x (cdr (assoc x a-recorder :test #'equal)))
                             (cons x "N/A"))) index)))

(defun write-data-to-file (hash-table)
  (with-open-file (s output-file :direction :output :if-does-not-exist :create :if-exists :append)
    (maphash #'(lambda (key value)
                 (let* ((line (if (typep value 'list)
                                  (reduce #'(lambda(x y)
                                              (concatenate 'string (cdr x) ">>>" (cdr y))) value)
                                  nil)))
                   (progn
                     (snowh4r3-common::write-log (concatenate 'string "正在写入记录：" key "\n") :stdout nil)
                     (format s "~A~%" line))))
                 hash-table)))
