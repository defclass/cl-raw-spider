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
    ;;; category participle click-total
    (let* ((goods (make-instance 'goods)))
      (progn
        (copy-slot (source min-image headline belong-to)  goods gdobj)
        (setf (good-id goods) (get-universal-time))
        (setf (mall-url goods) (get-real-mall-url (mall-raw-url gdobj)))
        (setf (create-time goods) (- (get-universal-time) (encode-universal-time 0 0 0 01 01 1970)))
        (setf (content goods) (get-gd-content (content-url gdobj))))
      goods)))

    
;;;;主函数

(defun collect-gdindex-obj (url)
  (let* ((json (parse-guangdiu-index url))
         (jso (make-jso-obj json)))
    (if (jso-value "status" jso)
        (let ((data (jso-value "data" jso)))
          (loop for i in data
             collect (gdindex-factory i)))
        nil)))

(defun get-gd-content (url)
  (let* ((json (parse-guangdiu-content url))
         (jso (make-jso-obj json)))
    (if (jso-value "status" jso)
        (let ((data (jso-value "data" jso)))
          (jso-value "content" data))
        nil)))

  
;;;; 助手函数
(defun parse-guangdiu-index (url)
  (parse-html url "parse.guangdiu.index.js"))


(defun parse-guangdiu-content (url)
  (parse-html url "parse.guangdiu.content.js"))


(defun gdindex-factory (jso)
  " guangdiu.com jso对象 转化为obj"
  (when (typep jso 'st-json:jso)
    (make-instance 'guangdiu :headline (st-json:getjso "title" jso)
                   :mall-raw-url (st-json:getjso "mallRawUrl" jso)
                   :source (st-json:getjso "source" jso)
                   :content-url (concatenate 'string "http://www.guangdiu.com/" (st-json:getjso "contentUrl" jso))
                   :belong-to (st-json:getjso "belongTo" jso)
                   :min-image (st-json:getjso "minImg" jso))))



(defun get-real-mall-url (raw-url)
  " 由于guangdiu上的直达链接 有多个，不同的链接需要不同处理。
此函数包装这些链接的处理，统一返回商城url且去除不带推荐人ID "
  (when (or 
         (search "go.php?" raw-url)
         (search "taobao" raw-url))
    (when (search "go.php?" raw-url)
      (setf raw-url (find-mall-url (concatenate 'string "http://guangdiu.com/" raw-url))))
    (setf raw-url (cut-promote-id raw-url))
    raw-url))



(defun find-mall-url (raw-url)
  " 由网站上提供的跳转url来解析真正的商城url "
  (if (search "go.php?" raw-url)
      (let* ((return-str  (get-content raw-url))
             (reglist  (multiple-value-list (cl-ppcre:scan-to-strings "}\\((.+)\\)" return-str)))
             (data (cl-ppcre:split "," (elt (cadr reglist) 0))))
        (destructuring-bind (raw-jscript-str  num1 num2 vars init-num set) data
          (declare (ignore num1 num2 init-num set))
          (decode-mall-url raw-jscript-str vars)))
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


(defun decode-mall-url (raw-jscript-str vars)
  (let* ((raw-jscript-str (string-downcase (subseq raw-jscript-str 1 (- (length raw-jscript-str) 1))))
         (vars-list (cl-ppcre:split "\\|" (subseq vars 1 (- (length vars) 12)))))
    (loop for i in vars-list
       do (let* ((key (position i vars-list))
                 (key-in-36 (format nil "~36r" key)))
            (when (not (equal "" i))
              (setf raw-jscript-str (cl-ppcre:regex-replace-all  (concatenate 'string "\\b"
                                                                (string-downcase key-in-36)
                                                                "\\b")
                                                                raw-jscript-str i)))))
    (ppcre:scan-to-strings "https?:/{2}\\w[^']+" raw-jscript-str)))

(defun parse-html(url jscript-name)
  (let* ((script-path (concatenate 'string (config:c "js-path") jscript-name))
         (html-path (wget-data url))
         (json (node-parse script-path html-path)))
    (when json
     (delete-file html-path))
    json))


;;;;; todo
(defun cut-promote-id (url)
  " 删除掉链接中的推广ID "
;;  (common:write-log url))
  url)


(defun jso-value (key jso)
  " 从一个jso 对象中 获取值"
  (if (typep jso 'st-json:jso)
      (st-json:getjso key  jso)
      (error (concatenate 'string " 输入数据不是 jso 对象,key:" key ))))
  
(defun make-jso-obj(json)
  " 将一个json数据转化为jso对象"
  (if (eq :true (st-json:as-json-bool json))
      (st-json:read-json json)
      (error " 输入数据不是 json 格式")))
