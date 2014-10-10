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


(defmethod goods-factory((gdobj guangdiu) )
  " 通过gdobj提供的数据去生成goods的obj"
  (macrolet ((copy-slot (list target-obj source-obj)
               `(progn
                  ,@(loop for i in list
                       collect `(setf (,i ,target-obj) (,i ,source-obj))))))
    ;;; mall-url create-time content category participle click-total
    (let* ((goods (make-instance 'goods)))
      (progn
        (copy-slot (source min-image headline belong-to)  goods gdobj)
        (setf (good-id goods) (get-universal-time))))))

    
;;;;主函数

(defun collect-gdindex-obj (url)
  (let* ((json (parse-guangdiu-index url))
         (struct-data (when (eq :true (st-json:as-json-bool json))
                        (st-json:read-json json))))
    (if (and (typep struct-data 'st-json:jso)
             (= 1 (st-json:getjso "status" struct-data)))
        (let ((data (st-json:getjso "data" struct-data)))
          (loop for i in data
             collect (gdindex-to-obj i)))
        nil)))
         

               
(defun parse-guangdiu-index (url)
  (let* ((script-path (concatenate 'string (config:c "js-path") "parse.guangdiu.index.js"))
         (html-path (wget-data url))
         (json (node-parse script-path html-path)))
    (when json
      (delete-file html-path))
    json))

;;;; 助手函数

(defun gdindex-to-obj (jso)
  " guangdiu.com jso对象 转化为obj"
  (when (typep jso 'st-json:jso)
    (make-instance 'guangdiu :headline (st-json:getjso "title" jso)
                   :mall-raw-url (st-json:getjso "mallRawUrl" jso)
                   :source (st-json:getjso "source" jso)
                   :content-url (st-json:getjso "contentUrl" jso)
                   :belong-to (st-json:getjso "belongTo" jso)
                   :min-image (st-json:getjso "minImg" jso))))

(defun find-mall-url (raw-url)
  " 由网站上提供的url来解析真正的商城url "
  (if (search "go.php?" raw-url)
      (let* ((return-str  (get-content raw-url))
             (reglist  (multiple-value-list (cl-ppcre:scan-to-strings "}\\((.+)\\)" return-str)))
             (data (cl-ppcre:split "," (elt (cadr reglist) 0))))
        (destructuring-bind (url  num1 num2 vars init-num set) data
          (declare (ignore num1 num2 init-num set))
          (decode-mall-url url vars)))
      raw-url))

;;;;; 工具函数

(defun get-content (url &key (encode :utf8))
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


(defun decode-mall-url (url vars)
  (let* ((url (string-downcase (subseq url 1 (- (length url) 1))))
         (vars-list (cl-ppcre:split "\\|" (subseq vars 1 (- (length vars) 12)))))
    (loop for i in vars-list
       do (let* ((key (position i vars-list))
                 (key-in-36 (format nil "~36r" key)))
            (when (not (equal "" i))
              (setf url (cl-ppcre:regex-replace-all  (concatenate 'string "\\b"
                                                                (string-downcase key-in-36)
                                                                "\\b")
                                                                url i)))))
    (ppcre:scan-to-strings "https?:/{2}\\w[^']+" url)))
    
    
  







