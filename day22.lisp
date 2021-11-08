(defpackage :day22
  (:use :cl :aoc-misc aoc-coord :iterate)
  (:export main)
  (:import-from :cl-ppcre :scan-to-strings))

(in-package :day22)

(defun decode (line)
  (multiple-value-bind (match regs) (scan-to-strings "^\/dev\/grid\/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+\%$" line)
    (when match
      (destructuring-bind (x y size used avail) (map 'list #'parse-integer regs)
        (list (make-coord x y) size used avail)))))

(defun compare (used-a avail-a)
  (if (zerop used-a)
    (lambda (cnt b)
      (destructuring-bind (_ _ used-b avail-b) b
        (if (>= avail-a used-b) (1+ cnt) cnt)))
    (lambda (cnt b)
      (destructuring-bind (_ _ used-b avail-b) b
        (+
          cnt
          (if (and (not (zerop used-b)) (>= avail-a used-b)) 1 0)
          (if (>= avail-b used-a) 1 0))))))

(defun find-viable-pairs (nodes &optional (cnt 0))
  (if (null nodes)
    cnt
    (destructuring-bind (_ _ used-a avail-a) (car nodes)
      (find-viable-pairs
        (cdr nodes)
        (reduce (compare used-a avail-a) (cdr nodes) :initial-value cnt)))))

(defun sort-by-coord (a b)
  (destructuring-bind ((c _ _ _) . (d _ _ _)) (cons a b)
    (let ((diff (- (get-y c) (get-y d))))
      (cond
        ((minusp diff) t)
        ((zerop  diff) (< (get-x c) (get-x d)))))))

(defun main ()
  (let
    ((input (cddr (read-input-as-list 22 #'decode))))
    (print (find-viable-pairs input))))

