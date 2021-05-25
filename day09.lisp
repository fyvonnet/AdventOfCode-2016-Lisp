(defpackage :day09
  (:use :cl :aoc-misc :iterate)
  (:import-from :cl-ppcre :scan-to-strings)
  (:export main))

(in-package :day09)

(defun read-marker (lst &optional mrk-lst)
  (if (char= #\) (car lst))
    (multiple-value-bind (match regs)
      (scan-to-strings "\\((\\d+)x(\\d+)" (coerce (reverse mrk-lst) 'string))
      (values
        (parse-integer (aref regs 0))
        (parse-integer (aref regs 1))
        (cdr lst)))
    (read-marker (cdr lst) (cons (car lst) mrk-lst))))

(defun drop (n lst)
  (if (zerop n)
    lst
    (drop (1- n) (cdr lst))))

(defun decompress-length (lst &optional (length 0))
  (cond
    ((null lst) length)
    ((char= #\( (car lst))
     (multiple-value-bind (nchar repeat new-lst) (read-marker lst)
       (decompress-length 
         (drop nchar new-lst)
         (+ length (* repeat nchar)))))
    (t (decompress-length (cdr lst) (1+ length)))))

(defun main ()
  (let
    ((str (coerce (car (read-input-as-list 09)) 'list)))
    (print (decompress-length str))))

