(cl:in-package :daogou)
(defparameter origin-url "http://guangdiu.com/")
;;;;定义类
(defclass guangdiu ()
  )
(defclass goods()
  )

;;;;主函数
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
    
    
            

              
            
              
                                  
                                  
                                  
          
                   
