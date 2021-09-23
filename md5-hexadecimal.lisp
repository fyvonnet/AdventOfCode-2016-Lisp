(defpackage :md5-hexadecimal
  (:use :cl)
  (:import-from :sb-md5 :md5sum-string)
  (:import-from :serapeum :nlet)
  (:export :md5-hexadecimal))

(in-package :md5-hexadecimal)

(defparameter *hex-syms* "0123456789abcdef")

(defun md5-hexadecimal (str)
  (let ((md5-str (make-string 32)))
    (nlet rec ((i 0) (md5-lst (coerce (md5sum-string str) 'list)))
      (if (null md5-lst)
        md5-str
        (progn
          (multiple-value-bind (q r) (floor (car md5-lst) 16)
            (setf (aref md5-str     i)  (aref *hex-syms* q))
            (setf (aref md5-str (1+ i)) (aref *hex-syms* r)))
          (rec (+ 2 i) (cdr md5-lst)))))))

