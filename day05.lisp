(defpackage :day05
  (:use :cl :aoc-misc)
  (:import-from :sb-md5 :md5sum-string)
  (:export main))

(in-package :day05)

(defun find-password (id &optional (index 0) (n 8))
  (unless (zerop n)
    (let ((md5sum (md5sum-string (format nil "~a~a" id index))))
      (if
        (and
          (zerop (aref md5sum 0))
          (zerop (aref md5sum 1))
          (< (aref md5sum 2) 16))
        (cons
          (char "0123456789abcdef" (aref md5sum 2))
          (find-password id (1+ index) (1- n)))
        (find-password id (1+ index) n)))))

(defun main ()
  (let
    ((input (car (read-input-as-list 05))))
    (format t "~a~%" (coerce (find-password input) 'string))))

