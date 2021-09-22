(defpackage :day16
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day16)

(defparameter *disk-length* 272)

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

(defun transform (lst)
  (append
    lst
    '(#\0)
    (invert lst)))

(defun fill-disk (lst)
  (if (>= (length lst) *disk-length*)
    (subseq lst 0 *disk-length*)
    (fill-disk (transform lst))))

(defun checksum-iteration (lst)
  (unless (null lst)
    (cons
      (if (char= (car lst) (cadr lst)) #\1 #\0)
      (checksum-iteration (cddr lst)))))

(defun checksum (lst)
  (if (zerop (mod (length lst) 2))
    (checksum (checksum-iteration lst))
    (coerce lst 'string)))

(defun main ()
  (let
    ((input (coerce (car (read-input-as-list 16)) 'list)))
    (format t "~a~%" (checksum (fill-disk input)))))

