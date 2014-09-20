(cl:in-package :snowh4r3-email)

(defparameter *smtp* '((smtp-server nil)
                       (username nil)
                       (password nil)
                       (from-who nil)))
(defun do-send-email (to)
  (let ((message "Dear Sirs,
  How are you?
  We have obtained your address from  alibaba.com.Your plastic recycled resources are attractive to us.There are many plastics  we need  (including but not limited to): HDPE Bottle regrinde, milk bottle grinde,HDPE film grade ,LDPE or hdpe film Waste plastic ,and HDPE OR ldpe REPRO.
  I shall be glad if you will send me your catalogue together with pictures, quantity of goods, information and quotation to  main ports of mainland China.

Yours faithfully, Zilong Huang

My Email:zilong@sutherland-hk.com.
Hong Kong Sutherland trading co., LTD.
http://sutherland-hk.com/

Please do not reply to this e-mail.Any reply is welcome to the mailbox:zilong@sutherland-hk.com .
If you do not wish to receive e-mails from  in the future, please visit http://sutherland-hk.com/unsubscribe

"))
    (cl-smtp:send-email "smtp.163.com" "icsendmail@163.com" to "Your product is what we need." message :authentication '("icsendmail@163.com" "LGt6xgFI"))))

(defun send-emails ()
  (let  ((emails (return-emails)))
    (loop
       for i in emails
         do (progn
              (do-send-email i)
              (format t "正在给 [~a] 发送邮件 ~%" i)
              (random-sleep)))))

(defun random-sleep ()
  (let ((time (+ 30 (random 50))))
    
  (sleep time)))

(defun return-emails ()
  (let* ((in (open "/home/hq/Desktop/europeam-plastic.cvs"))
         (emails (when in
                   (loop for line  = (read-line in nil)
                      while line  append (split-into-email (third (split-by-delimiter line #/,)))))))
    (close in)
    (append (list "mike05@qq.com")  emails)))

(defun split-by-delimiter (string delimiter)
  "Returns a list of substrings of string
divided by delimiter  each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position delimiter string :start i)
     collect (subseq string i j)
     while j))

(defun split-into-email (str)
  (let ((emails (butlast (split-by-delimiter str #\;) 1)))
    (loop
         for i in emails
         collect  (remove #\Space i))))

