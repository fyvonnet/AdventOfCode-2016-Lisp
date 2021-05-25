(defpackage :day09
  (:use :cl :aoc-misc :iterate)
  (:export main))

(in-package :day09)

(defun read-integer (str &optional (value 0))
  (destructuring-bind (fst . rst) str
    (if (digit-char-p fst)
      (read-integer rst (+ (* 10 value) (- (char-code fst) 48)))
      (values value rst))))

(defun read-marker (lst &optional mrk-lst)
  (multiple-value-bind (nchar temp-rst) (read-integer (cdr lst))
    (multiple-value-bind (repeat rst) (read-integer temp-rst)
      (values nchar repeat rst))))

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

