(cl:in-package :daogou)

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
  ((good-id :accessor good-id)
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
  ((guangdiu :initform '() :accessor guangdiu)
   (raw-ggs :initform '() :accessor raw-ggs
               :documentation "此slot是未判断新旧的guangdiu对象")
   (goods :initform '() :accessor goods)))

(defvar *collect* (make-instance 'collect)
  " 全局变量*collect* 收集 goods 对象和guangdiu对象 ")

(defvar *connect* nil
  " Mysql 连接的全局变量")

(defparameter base-url "http://guangdiu.com/index.php?p=")



;;; 主方法
(defmethod collect-goods ((collect collect))
  " 将 goods 对象存入 *collect* 中"
  (let ((gdobj-list (reverse (guangdiu collect))))
    (when gdobj-list
      (progn
        (loop for gdobj in gdobj-list
           do (let ((good (create-good gdobj)))
               (when good
                 (setf (goods collect)
                       (append (goods collect)
                               (list good))))))
        t))))

(defmethod save-goods ((collect collect))
  " 对 *collect*中的goods对象保存到数据库 "
  (let ((goods (goods collect))
        (dividing-obj (car (guangdiu *collect*))))
    (loop for i in goods
       do (save-good i))
    (when dividing-obj
      (save-gg-dividing-obj (car (guangdiu *collect*))))))

;;; 辅助方法
(defmethod create-good ((gdobj guangdiu))
  " 通过gdobj提供的数据去生成goods的obj"
  (macrolet ((copy-slot (list target-obj source-obj)
               `(progn
                  ,@(loop for i in list
                       collect `(setf (,i ,target-obj) (,i ,source-obj))))))
    ;;; category participle click-total 未设定
    (let* ((goods (make-instance 'goods)))
      (handler-case 
          (progn
            (copy-slot (source min-image headline belong-to)  goods gdobj)
            (setf (good-id goods) (get-universal-time))
            (setf (mall-url goods) (get-real-mall-url (mall-raw-url gdobj)))
            (setf (create-time goods) (- (get-universal-time) (encode-universal-time 0 0 0 01 01 1970)))
            (setf (content goods) (get-gd-content (content-url gdobj)))
            goods)
        (failed-to-get-content (cond-obj)
          (format t "获取内容失败,失败信息:~A 放弃重试~%" (get-content-failed-message cond-obj)))))))

(defmethod save-good ((goods goods))
  (if (not *CONNECT*)
      (connect)
      (with-slots ((slot-source source)
                   (min-image min-image)
                   (mall-url mall-url)
                   (slot-headline headline)
                   (create-time create-time)
                   (slot-belong-to belong-to)
                   (slot-content content)) goods
        (let (sql headline belong-to content source)
          (progn
            (setf headline (mysql-escape-string slot-headline))
            (setf belong-to (mysql-escape-string slot-belong-to))
            (setf content (mysql-escape-string slot-content))
            (setf source (mysql-escape-string slot-source))
            (setf sql (concatenate 'string " insert into `dg-good` (`source`,`min-image`,`mall-url`,`headline`,`create-time`,`belong-to`,`content`) values  ('" source "','" min-image "','" mall-url "','" headline "','" (write-to-string create-time) "','"  belong-to "','" content "');"))
            (handler-case 
                (cl-mysql:query sql)
              (COM.HACKINGHAT.CL-MYSQL-SYSTEM:MYSQL-ERROR ()
                (common::write-log (concatenate 'string "[error] sql执行失败: " sql)))))))))

(defvar *dividing-line* '(("guangdiu" . ()))
  " This var saved the dividing-line object")

(defmethod find-dividing-and-coll-gg ((coll collect))
  " 如果为T此次函数运行已经找到分隔对象不必再找下去，如果为nil则需要继续查找 "
  (with-slots ((gg-list raw-ggs)) coll
    (if (and gg-list
             (typep gg-list 'list))
        (let ((old-obj (get-gg-dividing-obj))
              position)
          (progn
            (loop for gg-obj in gg-list
               do (unless (new-gg-p gg-obj old-obj)
                    (setf position (position gg-obj gg-list))))
            (cond
              (position (progn
                          (setf gg-list (subseq gg-list 0 position)))
                          t)
              (t nil))))
        (error "*collect*对象slot:raw-ggs 值为nil或不是list类型,不能执行 new-gg方法"))))

(defmethod new-gg-p ((new-obj guangdiu) (old-obj guangdiu))
  " 如果两个对象有一个属性是相同的，则判断输入的新对象是旧的 "
  (not (or (equal (headline new-obj)
                  (headline old-obj))
           (equal (mall-raw-url new-obj)
                  (mall-raw-url old-obj))
           (equal (min-image new-obj)
                  (min-image old-obj))
           (equal (content-url new-obj)
                  (content-url old-obj)))))
              
;;;;主函数

(defun enter()
  (loop
     do (PROGN
          (main)
          (format t "休息1分钟~2%")
          (sleep 60))))
  

(defun main()
  (progn
    (init)
    (write-log "info:收集guangdiu")
    (collect-gd-obj base-url)
    (if (and (write-log "info:收集goods")
             (collect-goods *collect*))
        (PROGN
          (write-log "info:保存到mysql")
          (save-goods  *collect*)
          (write-log "info:保存到wordpress")
          (save-to-wordpress))
        (progn
          (write-log "info:没有要保存的goods")))))
          

(defun init()
  (PROGN
    (unless (get-gg-dividing-obj)
      (write-log "info:初始化dividing")
      (init-dividing))
    ;; init *collect*
    (setf *collect* (make-instance 'collect))))
    
(defun collect-gd-obj (base-url)
  (loop
     with flag = t
     with i = 1
     while flag
     do (let ((index-url (concatenate
                          'string base-url (write-to-string i)))
              (gg-objs nil))
          (progn
            (setf i (+ i 1))
            (setf gg-objs (gd-index-to-obj index-url))
            (setf (raw-ggs *collect*) gg-objs)
            (when (find-dividing-and-coll-gg *collect*)
              (setf flag nil))
            (setf (guangdiu *collect*)
                  (append (guangdiu *collect*) (raw-ggs *collect*)))
            (setf (raw-ggs *collect*) nil)))))
                     
(defun gd-index-to-obj (url)
  " 通过逛丢列表页的url，来取得所有对象 返回值是list "
  (let* ((json (parse-guangdiu-index url))
         (jso (make-jso-obj json)))
    (if (jso-value "status" jso)
        (let ((data (jso-value "data" jso)))
          (loop for i in data
             collect (gdindex-factory i)))
        nil)))


(defun get-gd-content (url)
  " 获取gd详情页的内容，返回字符串 "
  (let* ((json (parse-guangdiu-content url))
         (jso (make-jso-obj json)))
    (if (jso-value "status" jso)
        (let ((data (jso-value "data" jso)))
          (jso-value "content" data))
        nil)))


;;;; 助手函数
(defun parse-guangdiu-index (url)
  " 解析逛丢的首页并返回json "
  (parse-html url "parse.guangdiu.index.js"))

(defun parse-guangdiu-content (url)
  " 解析逛丢的详情页并返回json "
  (parse-html url "parse.guangdiu.content.js"))

(defun save-to-wordpress()
  (common::sh (concatenate 'string "php " (c "php-path") "update-to-wordpress.php")))
  

(defun get-real-mall-url (raw-url)
  " 由于guangdiu上的直达链接 有多个，不同的链接需要不同处理。
此函数包装这些链接的处理，统一返回商城url且去除不带推荐人ID "
    (when (search "go.php?" raw-url)
      (PROGN
        (write-log (concatenate 'string "info:请求中转链接" raw-url))
        (setf raw-url (find-mall-url  raw-url))))
  raw-url)

(defun find-mall-url (raw-url)
  " 此函数为 get-real-mall-url 的工具函数，调用时使用get-real-mall-url
 由网站上提供的跳转url来解析真正的商城url "
  (if (search "go.php?" raw-url)
      (let* ((return-str  (get-content raw-url))
             (reglist  (multiple-value-list (cl-ppcre::scan-to-strings "}\\((.+)\\)" return-str)))
             (data (cl-ppcre::split "," (elt (cadr reglist) 0))))
        (destructuring-bind (raw-jscript-str  num1 num2 vars init-num set) data
          (declare (ignore num1 num2 init-num set))
          (decode-mall-url raw-jscript-str vars)))
      raw-url))

(defun gdindex-factory (jso)
  " guangdiu.com jso对象 转化为obj"
  (when (typep jso 'st-json:jso)
    (make-instance 'guangdiu
                   :headline (st-json:getjso "title" jso)
                   :mall-raw-url (st-json:getjso "mallRawUrl" jso)
                   :source (st-json:getjso "source" jso)
                   :content-url (concatenate 'string "http://www.guangdiu.com/" (st-json:getjso "contentUrl" jso))
                   :belong-to (st-json:getjso "belongTo" jso)
                   :min-image (st-json:getjso "minImg" jso))))

(defun init-dividing()
  (unless (get-gg-dividing-obj)
    (let (objs obj)
      (setf objs (gd-index-to-obj
                  (concatenate 'string base-url (write-to-string 1))))
      (setf obj (car objs))
      (save-gg-dividing-obj obj))))
    
      

(defun get-gg-dividing-obj ()
  " 获取 guangdiu 分界 obj"
  (read-dividing-var)
  (common::plist-value "guangdiu" *DIVIDING-LINE*))

(defun save-gg-dividing-obj (obj)
  (common::setf-plist "guangdiu" *dividing-line* obj)
  (save-dividing-var))

;;;;; 工具函数

(defun get-content (url &key (encode :utf-8))
  " 获取网页html内容 "
  (let* ((user-agent (elt common:user-agents (random (length common:user-agents))))
         binary)
    (handler-case
        (progn
          (setf binary (drakma::http-request url :force-binary t :user-agent user-agent))
          (sb-ext::octets-to-string binary :EXTERNAL-FORMAT encode))
      (USOCKET:NS-HOST-NOT-FOUND-ERROR ()
        (PROGN
          (format t "DNS解析错误重新获取:~%   url:~A ~%" url)
          (get-content url)))
      (USOCKET:TIMEOUT-ERROR ()
        (PROGN
          (format t "超时错误重新获取~% url:~A ~%" url)
          (get-content url)))
      (SB-IMPL::INVALID-UTF8-STARTER-BYTE ()
        (progn
          (format t "UTF8解码错误~% 尝试GBK解码~%")
          (get-content url :encode :gbk)))
      (SB-IMPL::MALFORMED-GBK ()
        (error 'failed-to-get-content :message "获取文本失败")))))
      
           
(defun wget-data(url)
  " 获取html文件并将其写入到/tmp/目录中"
  (let* ((html (progn
                 (format t "保存链接内容:~A~%" url)
                 (get-content url)))
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
  " 解密真实的url地址 "
  (let* ((raw-jscript-str (string-downcase (subseq raw-jscript-str 1 (- (length raw-jscript-str) 1))))
         (vars-list (cl-ppcre::split "\\|" (subseq vars 1 (- (length vars) 12))))
         raw-url)
    (progn
      (loop for i in vars-list
         do (let* ((key (position i vars-list))
                   (key-in-36 (format nil "~36r" key)))
              (when (not (equal "" i))
                (setf raw-jscript-str (cl-ppcre::regex-replace-all  (concatenate 'string "\\b"
                                                                                 (string-downcase key-in-36)
                                                                                 "\\b")
                                                                    raw-jscript-str i)))))
      (setf raw-url (ppcre::scan-to-strings "https?:/{2}\\w[^']+" raw-jscript-str))
      (subseq raw-url 0 (- (length raw-url) 1)))))

(defun parse-html(url jscript-name)
  " 通用函数，给定url 和 解析网页的js脚本 "
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

(defun connect ()
  (setf *connect*
        (cl-mysql:connect :host (c "db-host")
                          :user (c "db-user")
                          :password (c "db-passwd")))
  (cl-mysql:query (concatenate 'string "use " (c "db-name"))))

(defun mysql-escape-string (str)
  (if (not (typep str 'string))
      (progn
        (format t "~A~%" str)
        (error "function: mysql-escape-string var is not string"))
      (progn
        (setf str (cl-ppcre:regex-replace-all "'" str "\\\\'"))
        (setf str (cl-ppcre:regex-replace-all "\"" str "\\\\\"")))))
       
(defun save-dividing-var ()
  (cl-store::store *dividing-line* (c "dividing-line-filepath")))

(defun read-dividing-var ()
  (setf *dividing-line*
        (cl-store::restore (c "dividing-line-filepath"))))
  
                      
