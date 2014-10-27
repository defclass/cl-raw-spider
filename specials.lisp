(in-package :nixiwan)
;;;; 定义app文件夹
(defvar *app-path* nil "app root path")
(setf *app-path* (asdf:system-source-directory :daogou))

;;;; 初始化 log
(log:config :daily (merge-pathnames "log/nixiwan.%Y%m%d" *app-path*))


(log:trace "设置*db-info*, *path*变量")
(defvar *db-info* nil "Database username and so on")
(defvar *paths* nil "Node path ,php path and so on")


(log:trace "开始定义类")
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
