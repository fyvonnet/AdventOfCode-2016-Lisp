(defpackage :day20
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match))

(in-package :day20)

(defun search-holes (lst &optional (first-hole nil) (holes 0))
  (nlet rec ((lst (cdr lst)) (maxi (1+ (cadar lst))))
    (match lst
      ((cons (list a b) rst)
       (if (> a maxi)
         (search-holes lst (if first-hole first-hole maxi) (+ holes (- a maxi)))
         (rec rst (max (1+ b) maxi))))
      (nil (list first-hole holes)))))

(defun main ()
  (let
    ((input
       (sort
         (read-input-as-list
           20
           (lambda (l) (mapcar #'parse-integer (split "-" l))))
         (lambda (a b) (< (first a) (first b))))))
    (dolist (a (search-holes input)) (print a))))

