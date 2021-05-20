(defpackage :day05
  (:use :cl :aoc-misc)
  (:import-from :sb-md5 :md5sum-string)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day05)

(defun hexa-char (n)
  (char "0123456789abcdef" n))

(defun find-password (id)
  (let
    ((passwd1 (make-string 8 :initial-element #\#))
     (passwd2 (make-string 8 :initial-element #\#)))
    (nlet rec ((index 0) (n1 8) (n2 8))
      (if (zerop n2)
        (list passwd1 passwd2)
        ; md5sum-string returns the MD5 hash as an array of 8-bit integers
        (let ((md5sum (md5sum-string (format nil "~a~a" id index))))
          (if
            (and
              (zerop (aref md5sum 0))  ; two first characters are zero
              (zerop (aref md5sum 1))  ; two next characters are zero
              (< (aref md5sum 2) 16))  ; 5th character is zero
            (let 
              ((a (aref md5sum 2))              ; 6th character
               (b (floor (aref md5sum 3) 16)))  ; 7th character
              (rec
                (1+ index)
                (if (zerop n1)
                  n1
                  (progn
                    (setf (aref passwd1 (- 8 n1)) (hexa-char a))
                    (1- n1)))
                (if (and (< a 8) (char= (aref passwd2 a) #\#))
                  (progn
                    (setf (aref passwd2 a) (hexa-char b))
                    (1- n2))
                  n2)))
            (rec (1+ index) n1 n2)))))))

(defun main ()
  (dolist (p (find-password (car (read-input-as-list 05))))
    (format t "~a~%" p)))

