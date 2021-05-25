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

(defun split (n lst &optional head)
  (if (zerop n)
    (values lst (reverse head))
    (split (1- n) (cdr lst) (cons (car lst) head))))

(defun decompress-length-part1 (lst &optional (length 0))
  (cond
    ((null lst) length)
    ((char= #\( (car lst))
     (multiple-value-bind (nchar repeat new-lst) (read-marker lst)
       (decompress-length-part1 
         (split nchar new-lst)
         (+ length (* repeat nchar)))))
    (t (decompress-length-part1 (cdr lst) (1+ length)))))

(defun decompress-length-part2 (lst &optional (length 0))
  (cond
    ((null lst) length )
    ((char= #\( (car lst))
     (multiple-value-bind (nchar repeat new-lst) (read-marker lst)
       (multiple-value-bind (tail head) (split nchar new-lst)
         (let ((sub-length (decompress-length-part2 head)))
           (decompress-length-part2 tail (+ length (* repeat sub-length)))))))
    (t (decompress-length-part2 (cdr lst) (1+ length)))))

(defun main ()
  (let
    ((str (coerce (car (read-input-as-list 09)) 'list)))
    (print (decompress-length-part1 str))
    (print (decompress-length-part2 str))))

