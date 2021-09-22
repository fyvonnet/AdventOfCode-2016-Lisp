(defpackage :day16
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day16)

(defun invert (lst &optional new-lst)
  (if (null lst)
    new-lst
    (invert
      (cdr lst)
      (cons
        (case (car lst)
          (#\0 #\1)
          (#\1 #\0))
        new-lst))))

(defun fill-disk (lst disk-length)
  (if (>= (length lst) disk-length)
    (subseq lst 0 disk-length)
    (fill-disk 
      (append lst '(#\0) (invert lst))
      disk-length)))

(defun checksum-iteration (lst &optional new-lst)
  (if (null lst)
    (reverse new-lst)
    (checksum-iteration
      (cddr lst)
      (cons
        (if (char= (car lst) (cadr lst)) #\1 #\0)
        new-lst))))

(defun checksum (lst)
  (if (zerop (mod (length lst) 2))
    (checksum (checksum-iteration lst))
    (coerce lst 'string)))

(defun main ()
  (let
    ((input (coerce (car (read-input-as-list 16)) 'list)))
    (dolist (dl '(272 35651584))
      (format t "~a~%" (checksum (fill-disk input dl))))))

