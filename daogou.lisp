(cl:in-package :nixiwan)
;;;;主函数
(defun enter()
  (loop
     do (PROGN
          (main)
          (log:info "休息1分钟~2%")
          (sleep 60))))

(defun main()
  (progn
    (log:info "初始化值")
    (init)
    (log:info "收集guangdiu")
    (collect-gd-obj base-url)
    (log:info "收集goods")
    (if (not (= (length *guangdiu-object-array*) 0))
        (PROGN
          (collect-goods *guangdiu-object-array*)
          (log:info "保存到mysql")
          (save-goods  *good-object-array*)
          (log:info "保存到wordpress")
          (save-to-wordpress))
        (progn
          (log:info "没有要保存到wordpress的内容")))))
          
(defun init()
  (PROGN
    (unless (get-gg-dividing-obj)
      (log:info "初始化dividin")
      (init-dividing))
    ;; init *collect*
    (setf *guangdiu-object-array* (make-adjustable-vector))
    (setf *good-object-array* (make-adjustable-vector))))


(defun collect-goods (gd-objs)
  " 将 goods 对象存入 *collect* 中"
  (if (typep gd-objs 'vector)
      (let ((gdobj-list (reverse gd-objs)))
        (when gdobj-list
          (loop for gdobj being  the elements of  gdobj-list
             do (let ((good (create-good gdobj)))
                  (when good
                    (vector-push-extend good *good-object-array*))))))
      (progn
        (log:error "函数参数错误")
        (error 'collect-goods-arg-error :message "函数参数错误"))))


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
            (setf (mall-url goods) (get-mall-url (mall-raw-url gdobj)))
            (setf (create-time goods) (- (get-universal-time) (encode-universal-time 0 0 0 01 01 1970)))
            (setf (content goods) (get-gd-content (content-url gdobj)))
            goods)
        (failed-to-get-content (cond-obj)
          (log:error "获取内容失败,失败信息:~A 放弃重试~%" (get-content-failed-message cond-obj)))))))

(defun save-goods (goods)
  " 对 *collect*中的goods对象保存到数据库 "
  (let ((dividing-obj (elt *guangdiu-object-array* 0)))
    (progn 
    (loop for i being the elements of goods
       do (progn
            (log:debug "~A" (headline i))
            (save-good i)))
    (log:debug "dividing obj: ~A" dividing-obj)
    (log:debug "bool :~A" (not dividing-obj))
    (when dividing-obj
        (save-gg-dividing-obj dividing-obj)))))

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
            (log:debug "slot-headline:~A" slot-headline)
            (log:debug "headline:~A" headline)
            (setf belong-to (mysql-escape-string slot-belong-to))
            (setf content (mysql-escape-string slot-content))
            (setf source (mysql-escape-string slot-source))
            (setf sql (concatenate 'string " insert into `dg-good` (`source`,`min-image`,`mall-url`,`headline`,`create-time`,`belong-to`,`content`) values  ('" source "','" min-image "','" mall-url "','" headline "','" (write-to-string create-time) "','"  belong-to "','" content "');"))
            (log:debug "~A" sql)
            (handler-case 
                (cl-mysql:query sql)
              (COM.HACKINGHAT.CL-MYSQL-SYSTEM:MYSQL-ERROR ()
                (log:error "sql执行失败: " sql))))))))
    
(defun collect-gd-obj (base-url)
  " 收集 guangdiu obj 的主入口"
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
            (multiple-value-bind (position available-objs)
                (find-dividing-and-coll-gg gg-objs)
              (unless  (eql position nil)
                (setf flag nil))
              (setf *guangdiu-object-array*
                    (concatenate 'vector *guangdiu-object-array* available-objs)))))))

(defun gd-index-to-obj (url)
  " 通过逛丢列表页的url，来取得所有对象 返回值是list "
  (let* ((json (parse-guangdiu-index url))
         (jso (make-jso-obj json)))
    (if (jso-value "status" jso)
        (let ((data (jso-value "data" jso))
              (objs (make-adjustable-vector)))
          (loop for i in data
             do (vector-push-extend (gdindex-factory i) objs))
          objs)
        nil)))

(defun get-gd-content (url)
  " 获取gd详情页的内容，返回字符串 "
  (let* ((json (parse-guangdiu-content url))
         (jso (make-jso-obj json)))
    (if (jso-value "status" jso)
        (let ((data (jso-value "data" jso)))
          (jso-value "content" data))
        nil)))

;;;; 辅助函数
(defun parse-guangdiu-index (url)
  " 解析逛丢的列表页并返回json "
  (log:info "开始解析 guangdiu 列表页 的内容" url)
  (parse-html url "parse.guangdiu.index.js"))

(defun parse-guangdiu-content (url)
  " 解析逛丢的详情页并返回json "
  (log:info "开始解析 guangdiu 详情页 的内容" url)
  (parse-html url "parse.guangdiu.content.js"))

(defun save-to-wordpress()
  (progn
    (log:debug "~A" (concatenate 'string "/usr/bin/php "
                            (namestring (get-path :php-path))
                            "update-to-wordpress.php"))
    (sh (concatenate 'string "/usr/bin/php "
                     (namestring (get-path :php-path))
                     "update-to-wordpress.php"))))
  

(defun get-mall-url (raw-url)
  " 由于guangdiu上的直达链接 有多个，不同的链接需要不同处理。
此函数包装这些链接的处理，统一返回商城url且去除不带推荐人ID "
  (if (typep raw-url 'string)
        (cond
          ((search "go.php?" raw-url)
         (PROGN
           (log:info "请求内部链接:~A" raw-url)
           (let*  ((mall-url-and-origin-promoteid (do-get-mall-url  raw-url)))
             (when mall-url-and-origin-promoteid
               (replace-promote-id mall-url-and-origin-promoteid)))))
          (t raw-url))
        raw-url))
                   

(defun do-get-mall-url (raw-url)
  " 此函数为 get-mall-url 的工具函数，调用时使用get-mall-url
 由网站上提供的跳转url来解析真正的商城url "
  (if (search "go.php?" raw-url)
      (let* ((return-str  (get-content raw-url))
             (reglist  (multiple-value-list (cl-ppcre::scan-to-strings "}\\((.+)\\)" return-str)))
             (data (destructuring-js-var (elt (cadr reglist) 0))))
        (destructuring-bind (raw-jscript-str  num1 num2 vars init-num) data
          (declare (ignore num1 num2 init-num ))
          (log:trace "开始解析二次获取返加的字符串:~A" raw-url)
          (decode-mall-url raw-jscript-str vars)))
      raw-url))

(defun destructuring-js-var (str)
  " 解析 js 的参数列表"
  (when (typep str 'string)
    (let (vars-list)
      (loop for char across str
         with var = (make-adjustable-string "")
         with quote-flag = nil
         with pre-char
         do (cond
              ((and (char= char #\,)
                    (not (find quote-flag "'\"")))
               (progn
                 (setf vars-list (append vars-list (list var)))
                 (setf var (make-adjustable-string ""))
                 (setf quote-flag nil)
                 (setf pre-char nil)))
              (t (progn
                   (when (find char "'\"")
                     (cond
                       ((not quote-flag) (setf quote-flag char))
                       ((and (char= char quote-flag)
                             (char/= pre-char #\\)) (setf quote-flag nil))))
                   (vector-push-extend char var)
                   (setf pre-char char)))))
      vars-list)))
                                              

(defun gdindex-factory (jso)
  " guangdiu.com jso对象 转化为obj"
  (when (typep jso 'st-json::jso)
    (apply #'make-instance
           'guangdiu
           (iter
             (for slot in (closer-mop:class-slots
                           (progn (closer-mop:ensure-finalized (find-class 'guangdiu))
                                  (find-class 'guangdiu))))
             (for  slot-name = (closer-mop:slot-definition-name slot))
             (when (jso-value  (symbol-munger:lisp->camel-case slot-name) jso)
               (progn
                 (collect (symbol-munger:lisp->keyword slot-name))
                 (collect (jso-value  (symbol-munger:lisp->camel-case slot-name) jso))))))))


(defun init-dividing()
  (unless (get-gg-dividing-obj)
    (let (objs obj)
      (setf objs (gd-index-to-obj
                  (concatenate 'string base-url (write-to-string 1))))
      (setf obj (car objs))
      (save-gg-dividing-obj obj))))

(defun get-gg-dividing-obj ()
  " 获取 guangdiu 分界 obj"
  (progn
    (setf *dividing-line*
          (cl-store::restore (get-path :dividing-line-filepath)))
    (log:info "读取dividing:~A"
              (headline (plist-value "guangdiu" *DIVIDING-LINE*))))
    (plist-value "guangdiu" *DIVIDING-LINE*))

(defun save-gg-dividing-obj (obj)
  " 保存 divinding"
  (progn
  (log:info "保存dividing" (headline obj))
  (setf-plist "guangdiu" *dividing-line* obj)
  (cl-store::store *dividing-line* (get-path :dividing-line-filepath))))


(defun find-dividing-and-coll-gg (aindex-guangdiu-vector)
  " 如果为T此次函数运行已经找到分隔对象不必再找下去，如果为nil则需要继续查找 "
  (if (and aindex-guangdiu-vector
           (typep aindex-guangdiu-vector 'vector))
      (let ((old-obj (get-gg-dividing-obj))
            position)
        (progn
          (loop for gg-obj being the elements of  aindex-guangdiu-vector
             do (unless (new-gg-p gg-obj old-obj)
                  (setf position (position gg-obj aindex-guangdiu-vector))))
          (values position
                  (subseq aindex-guangdiu-vector 0 position))))
      (progn
        (log:error "参数不是向量或者为空")
        (error 'find-dividing-arg-error :message "参数不是向量或者为空"))))

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

;;;; 工具函数
(defun get-content (url &key (encode :utf-8))
  " 获取网页html内容 "
  (let* ((user-agent (elt user-agents (random (length user-agents))))
         binary)
    (handler-case
        (progn
          (setf binary (drakma::http-request url :force-binary t :user-agent user-agent))
          (sb-ext::octets-to-string binary :EXTERNAL-FORMAT encode))
      (USOCKET:NS-HOST-NOT-FOUND-ERROR ()
        (PROGN
          (log:warn "DNS解析错误重新获取: url:~A " url)
          (get-content url)))
      (USOCKET:TIMEOUT-ERROR ()
        (PROGN
          (log:warn "超时错误重新获取~% url:~A" url)
          (get-content url)))
      (SB-IMPL::INVALID-UTF8-STARTER-BYTE ()
        (progn
          (log:warn "UTF8解码错误, 尝试GBK解码")
          (get-content url :encode :gbk)))
      (SB-IMPL::MALFORMED-GBK ()
        (error 'failed-to-get-content :message "获取文本失败"))
      (CHUNGA:INPUT-CHUNKING-UNEXPECTED-END-OF-FILE ()
        (PROGN
          (log:warn  "错误：INPUT-CHUNKING-UNEXPECTED-END-OF-FILE,尝试重新获取")
          (get-content url))))))



(defun wget-data(url)
  " 获取html文件并将其写入到/tmp/目录中"
  (let* ((html (progn
                 (log:info "保存链接内容:~A" url)
                 (get-content url)))
         (html-path (concatenate 'string (get-path :data-path)
                                 (write-to-string (get-universal-time)))))
    (if html
        (progn
          (log::info "正在写入文件:~A" html-path)
          (with-open-file (s html-path :direction :output :if-exists :append :if-does-not-exist :create)
            (format s "~A~%" html)
            html-path))
        nil)))


(defun node-parse (script-path html-path)
  " 解析文件的路径与html的路径 "
  (let* ((node-path (get-path :node-bin-path))
         (cmd (concatenate 'string node-path " " (namestring script-path) " " (namestring html-path))))
    (log:trace "执行解析网页动作,cmd:~A" cmd)
    (sh cmd)))


(defun decode-mall-url (raw-jscript-str vars)
  (let* ((raw-jscript-str
          (handler-case 
              (subseq raw-jscript-str 1 (- (length raw-jscript-str) 1))
            (error (e) (PROGN (log:error "raw-jscript-str解析错误,错误为~A" e)
                              (error 'decode-mall-url-error :message "raw-jscript-str解析错误")))))
         (vars-list
          (handler-case
              (cl-ppcre::split "\\|" (subseq vars 1 (- (length vars) 12)))
            (error (e) (PROGN (log:error "vars-list解析错误,错误为~A" e)
                              (error 'decode-mall-url-error :message "vars-list解析错误")))))
         js-script raw-url)
    (labels ((decode-match (match)
               (let ((char (elt match 0)))
                 (if (and (alpha-char-p char)
                          (upper-case-p char))
                     (progn
                       (log:trace "进入大写的处理方案,字符为~A" char)
                       (- (char-int char) 29))
                     (progn
                       (log:trace "进入36进制的处理方案,字符为~A" char)
                       (digit-char-p char 36)))))
             (replace-fun (match vars-list)
               (let* ((index (decode-match match))
                      (replacement (if (or (>= index (length vars-list))
                                           (not (elt vars-list index)))
                                       match
                                       (elt vars-list index))))
                 (if (not (string= replacement ""))
                     replacement
                     match))))
      (progn
        (log:trace "raw-jscript-str为:~A" raw-jscript-str)
        (setf js-script (cl-ppcre::regex-replace-all "\\b[0-9a-zA-Z]\\b"
                                                     raw-jscript-str
                                                     #'(lambda (match)
                                                         (replace-fun match vars-list))
                                                     :simple-calls t))
        (log:trace "js-script为:~A" js-script)
        (when js-script
          (progn
            (setf raw-url (ppcre::scan-to-strings "https?:/{2}\\w[^']+" js-script))
            (log:trace "解析出的raw-url为:~A" raw-url)
            (when raw-url
              (subseq raw-url 0 (- (length raw-url) 1)))))))))

(defun parse-html(url jscript-name)
  " 通用函数，给定url 和 解析网页的js脚本 "
  (let* ((script-path (merge-pathnames (get-path :js-path) jscript-name))
         (html-path (wget-data url))
         (json (node-parse script-path html-path)))
    (when json
      (delete-file html-path))
    json))


(defun replace-promote-id (url)
  " 替换掉链接中的推广ID "
  (if url
      (let (my-mall-url)
        (progn 
          (setf my-mall-url
                (cond
                  ((search "duomai" url) (replace-duomai-promote-id url))
                  (t url)))
          (log:info"promote-id替换成:~s" my-mall-url))
          (replace-duomai-promote-id my-mall-url))
      (progn
        (log:error "url为:~A,无法替换promote-id" url)
        (error 'replace-promote-empty-url :message "replace-promote-empty-url"))))

(defun replace-duomai-promote-id(url)
  " 替换多麦推广平台上的链接 "
  (if (and url (typep url 'string) (search "duomai" url))
      (cl-ppcre::regex-replace-all  "site_id=\\d+" url "site_id=135082")
      url))

(defun jso-value (key jso)
  " 从一个jso 对象中 获取值"
  (if (typep jso 'st-json:jso)
      (st-json:getjso key  jso)
      (progn
        (log:error "输入数据不是 jso 对象,key:~A" key)
        (error 'json-error :message  (concatenate 'string "输入数据不是 jso 对象,key:" key )))))

(defun make-jso-obj(json)
  " 将一个json数据转化为jso对象"
  (if (eq :true (st-json:as-json-bool json))
      (st-json:read-json json)
      (progn
        (log:error "输入数据不是 json 格式:~A" json)
        (error 'json-error :message  "输入数据不是 json 格式"))))

;; (defun connect ()
;;   (progn
;;     (crane:setup
;;      :databases
;;      `(:main
;;        (:type :mysql
;;               :name ,(get-db-info :db-name)
;;               :user ,(get-db-info :db-user)
;;               :pass ,(get-db-info :db-passwd))))
;;     (setf *CONNECT* (crane:connect))))

(defun connect ()
  (setf *connect*
        (cl-mysql:connect :host (get-db-info :db-host)
                          :user (get-db-info :db-user)
                          :password (get-db-info :db-passwd)))
  (cl-mysql:query (concatenate 'string "use " (get-db-info :db-name))))

  
(defun mysql-escape-string (str)
  (if (not (typep str 'string))
      (progn
        (log:error "输入数据不是string格式:~A" str)
        (error 'mysql-string-empty :message "输入数据不是string格式"))
      (progn
        (setf str (cl-ppcre:regex-replace-all "'" str "\\\\'"))
        (log:trace "转义'后的" str)
        (setf str (cl-ppcre:regex-replace-all "\"" str "\\\\\""))
        (log:trace "转义\\后的" str)
        str)))
       
