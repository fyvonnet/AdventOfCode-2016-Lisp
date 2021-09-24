(defpackage :day18
  (:use :cl :aoc-misc)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:export main))

(in-package :day18)

(defun decode (line)
  (nlet rec ((lst (coerce line 'list)) (output-lst nil))
    (if (null lst)
      (reverse output-lst)
      (rec
        (cdr lst)
        (cons
          (case (car lst)
            (#\. nil)
            (#\^ t))
          output-lst)))))

(defun next-line (line)
  (nlet rec ((lst (concatenate 'list '(nil) line '(nil))) (output-lst nil))
    (if (< (length lst) 3)
      (reverse output-lst)
      (rec
        (cdr lst)
        (cons
          (match (subseq lst 0 3)
            ((list t   t   nil) t)
            ((list nil t   t  ) t)
            ((list t   nil nil) t)
            ((list nil nil t  ) t)
            (_ nil))
          output-lst)))))

(defun count-safe (lst)
  (if (null lst)
    0 
    (+ (if (car lst) 0 1) (count-safe (cdr lst)))))

(defun count-all-safe-tiles (line countdown)
  (if (zerop countdown)
    0
    (+
      (count-safe line)
      (count-all-safe-tiles (next-line line) (1- countdown)))))

(defun main ()
  (let
    ((input (car (read-input-as-list 18 #'decode))))
    (dolist (n '(40 400000))
      (print (count-all-safe-tiles input n)))))

