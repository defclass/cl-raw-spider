(cl:in-package :daogou)
(defparameter origin-url "http://guangdiu.com/")

;;;;定义类
(defclass guangdiu ()
  ((headline :initform "" :initarg :headline :accessor headline)
   (mall-raw-url :initform "" :initarg :mall-raw-url :accessor mall-raw-url)
   (min-image :initarg :min-image :accessor min-image)
   (source :initform "" :initarg :source :accessor source)
   (content-url :initarg :content-url :accessor content-url :documentation "内容页的网址")
   (belong-to :initform 0 :initarg :belong-to :accessor belong-to)
   (category :initform "" :initarg :content :accessor category)
   (participle :initform "" :initarg  :participle :accessor participle)))
  
(defclass goods()
  ((good-id :initform (get-universal-time) :accessor good-id)
   (source :initform "" :initarg source :accessor source)
   (min-image :initarg :min-image :accessor min-image)
   (headline :initform "" :initarg :headline :accessor headline)
   (mall-url :initform "" :initarg :mall-url :accessor mall-url)
   (create-time :initform 0 :initarg :create-time :accessor create-time)
   (belong-to :initform 0 :initarg :belong-to :accessor belong-to)
   (content :initform "" :initarg :content :accessor content)
   (category :initform "" :initarg :category :accessor category)
   (participle :initform "" :initarg  :participle :accessor participle :documentation "分词")
   (click-total :initform 0 :accessor click-total)))


(defclass collect()
  ((guangdiu :initform () :accessor guangdiu)
   (goods :initarg () :accessor goods)))

   
;;;;主函数

(defun collect-gdindex-obj (url)
  (let* ((json (parse-guangdiu-index url))
         (struct-data (when (eq :true (st-json:as-json-bool json))
                        (st-json:read-json json 'list))))
    (if (and (typep struct-data 'st-json:jso)
             (= 1 (st-json:getjso "status" struct-data)))
        (let ((data (st-json:getjso "data" struct-data)))
          (loop for i in data
             collect (make-instance 'guangdiu :headline (st-json:getjso "title" struct-data)
                                                    :mall-raw-url (st-json:getjso "mallRawUrl" struct-data)
                                                    :source (st-json:getjso "source" struct-data)
                                                    :content-url (st-json:getjso "contentUrl" struct-data)
                                                    :belong-to (st-json:getjso "belongTo" struct-data)
                                                    :min-image (st-json:getjso "minImg" struct-data))))
        nil)))
         

               
(defun parse-guangdiu-index (url)
  (let* ((script-path (concatenate 'string (config:c "js-path") "parse.guangdiu.index.js"))
         (html-path (wget-data url))
         (json (node-parse script-path html-path)))
    (when json
      (delete-file html-path))
    json))

;;;; 辅助函数
(defun get-content (url &key (encode :utf8)
  (let* ((user-agent (elt common:user-agents (random (length common:user-agents)))))
    (drakma:http-request url :external-format-in encode :user-agent user-agent)))

(defun wget-data(url)
  " 获取html文件并将其写入到/tmp/目录中"
  (let* ((html (get-content url))
         (html-path (concatenate 'string (CONFIG:c "data-path")
                                 (write-to-string (get-universal-time)))))
    (if html
        (with-open-file (s html-path :direction :output :if-exists :append :if-does-not-exist :create)
          (format s "~A~%" html)
          html-path)
        nil)))
    
(defun node-parse (script-path html-path)
  " 解析文件的路径与html的路径 "
  (let* ((node-path (config:c "node-bin-path"))
         (cmd (concatenate 'string node-path " " script-path " " html-path)))
    (common::sh cmd)))
    
    
            

              
            
              
                                  
                                  
                                  
          
                   
