(defpackage :day22
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :cl-ppcre :scan-to-strings))

(in-package :day22)

(defun decode (line)
  (multiple-value-bind (match regs) (scan-to-strings "^\/dev\/grid\/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+\%$" line)
    (when match (map 'list #'parse-integer regs))))

(defun valid (avail used)
  (if (and (not (zerop used)) (>= avail used)) 1 0))

(defun compare (a)
  (lambda (cnt b)
    (destructuring-bind ((_ _ _ used-a avail-a) . (_ _ _ used-b avail-b)) (cons a b)
      (+ cnt (valid avail-a used-b) (valid avail-b used-a)))))

(defun find-viable-pairs (nodes)
  (if (null nodes)
    0
    (+
      (reduce (compare (car nodes)) (cdr nodes) :initial-value 0)
      (find-viable-pairs (cdr nodes)))))

(defun main ()
  (let
    ((input (remove-if #'null (read-input-as-list 22 #'decode)))a)
    (print (find-viable-pairs input))))

