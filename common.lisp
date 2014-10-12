(cl:in-package :common)
(defun write-log (str &key (stdout t)  )
  (with-open-file (s "~/Desktop/lisp.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (when stdout
          (format t "~A~%" str))
      (format s "~A~%" str)))

(defun read-file-to-str (path)
  (let* ((stream (open path)))
    (with-output-to-string (out)
      (loop
         (multiple-value-bind (line nl) (read-line stream nil stream)
           (when (eq line stream)
             (return))
           (write-string line out)
           (unless nl
             (write-char #\Newline out)))))))

(defun plist-value (key plist)
  (cdr (assoc key plist :test  #'string=)))

(defun setf-plist (key plist value)
  (setf (cdr (assoc key plist :test  #'string=))
        value))


(defun sh (cmd)
    #+clisp
        (let ((str (ext:run-shell-command cmd :output:stream)))
            (loop for line = (read-line str nil)
             until (null line)
             do (print line)))
    #+ecl
        (si:system cmd)
    #+sbcl
    (with-output-to-string (stream )
      (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output stream))
    #+clozure
        (ccl:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*))
