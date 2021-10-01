(defpackage :day19
  (:use :cl :aoc-misc)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day19)

(defun josephus (n k)
  (nlet rec ((a 1) (m 1))
    (if (> a n)
      (1+ m)
      (rec (1+ a) (mod (+ m k) a)))))

(defun main ()
  (let*
    ( (input (car (read-input-as-list 19 #'parse-integer))))
    (print (josephus input 2))))

