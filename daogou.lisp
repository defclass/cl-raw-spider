(cl:in-package :nixiwan)
;;;; 主函数



;;;; 辅助函数

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
                 (log:info "保存链接内容:~A~%" url)
                 (get-content url)))
         (html-path (concatenate 'string (get-path :data-path)
                                 (write-to-string (get-universal-time)))))
    (if html
        (with-open-file (s html-path :direction :output :if-exists :append :if-does-not-exist :create)
          (format s "~A~%" html)
          html-path)
        nil)))


(defun node-parse (script-path html-path)
  " 解析文件的路径与html的路径 "
  (let* ((node-path (get-path :node-bin-path))
         (cmd (concatenate 'string node-path " " script-path " " html-path)))
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
  (let* ((script-path (concatenate 'string (get-path :js-path) jscript-name))
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

(defun connect ()
  (progn
    (crane:setup
     :databases
     `(:main
       (:type :mysql
              :name ,(get-db-info :db-name)
              :user ,(get-db-info :db-user)
              :pass ,(get-db-info :db-passwd))))
    (crane:connect)))

  
(defun mysql-escape-string (str)
  (if (not (typep str 'string))
      (progn
        (log:error "输入数据不是string格式:~A" str)
        (error 'mysql-string-empty :message "输入数据不是string格式"))
      (progn
        (setf str (cl-ppcre:regex-replace-all "'" str "\\\\'"))
        (log:trace "转义'后的" str)
        (setf str (cl-ppcre:regex-replace-all "\"" str "\\\\\""))
        (log:trace "转义\\后的" str))))
       
(defun save-dividing-var ()
  (PROGN
    (log:trace "保存dividing")
    (cl-store::store *dividing-line* (get-path :dividing-line-filepath))))

(defun read-dividing-var ()
  (PROGN
    (log:trace "读取dividing")
    (setf *dividing-line*
          (cl-store::restore (get-path :dividing-line-filepath)))))
