(defpackage :day19
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day19)

; https://www.youtube.com/watch?v=uCsD3ZGzMgE
(defun josephus (n &optional (l 0) (bit-value 1))
  (if (= n 1)
    (1+ (* 2 l))
    (multiple-value-bind (q r) (floor n 2)
      (josephus q (+ l (* r bit-value)) (* 2 bit-value)))))

(defun main ()
  (let*
    ( (input (car (read-input-as-list 19 #'parse-integer))))
    (print (josephus input))))

