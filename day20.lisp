(defpackage :day20
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :split))

(in-package :day20)

(defun search-holes (lst &optional (maxi 0) (first-hole nil) (holes 0))
  (if (null lst)
    (list first-hole holes)
    (destructuring-bind ((lo hi) &rest rst) lst
     (if (> lo maxi)
       (search-holes rst (1+ hi) (if first-hole first-hole maxi) (+ holes (- lo maxi)))
       (search-holes rst (max (1+ hi) maxi) first-hole holes)))))

(defun main ()
  (let
    ((input
       (sort
         (read-input-as-list
           20
           (lambda (l) (mapcar #'parse-integer (split "-" l))))
         (lambda (a b) (< (first a) (first b))))))
    (dolist (a (search-holes input)) (print a))))

