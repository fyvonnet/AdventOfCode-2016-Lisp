(defpackage :day22
  (:use :cl :aoc-misc aoc-coord :iterate)
  (:export main)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :trivia :match))

(in-package :day22)

(defun decode (line)
  (multiple-value-bind (match regs) (scan-to-strings "^\/dev\/grid\/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+\%$" line)
    (when match
      (destructuring-bind (x y size used avail) (map 'list #'parse-integer regs)
        (list (make-coord x y) size used avail)))))

(defun compare-a-and-b (used-a avail-a)
  (lambda (cnt b)
    (destructuring-bind (_ _ used-b avail-b) b
      (+
        cnt
        (if (and (>= avail-a used-b) (not (zerop used-b))) 1 0)
        (if (>= avail-b used-a) 1 0)))))

(defun compare-only-b (avail-a)
  (lambda (cnt b)
    (destructuring-bind (_ _ used-b _) b
      (if (>= avail-a used-b) (1+ cnt) cnt))))

(defun find-viable-pairs (nodes &optional (cnt 0))
  (if (null nodes)
    cnt
    (find-viable-pairs
      (cdr nodes)
      (reduce
        (match (car nodes)
          ((list _ _      0 avail-a) (compare-only-b avail-a))
          ((list _ _ used-a avail-a) (compare-a-and-b used-a avail-a)))
        (cdr nodes)
        :initial-value cnt))))

(defun sort-by-coord (a b)
  (destructuring-bind ((c _ _ _) . (d _ _ _)) (cons a b)
    (let ((diff (- (get-y c) (get-y d))))
      (cond
        ((minusp diff) t)
        ((zerop  diff) (< (get-x c) (get-x d)))))))

(defun main ()
  (let
    ((input (cddr (read-input-as-list 22 #'decode))))
    (print (find-viable-pairs input))
    (let
      ((limits (get-coords-limits (mapcar #'car input)))
       (sorted-input (sort input #'sort-by-coord)))
      (terpri)
      (iterate
        (for y from 0 to (getf limits :y-max))
        (iterate
          (for x from 0 to (getf limits :x-max))
          (destructuring-bind (_ capa used _) (pop input)
            (if (> capa 500)
              (format t " XXXXX")
              (format t " ~2a/~2a" used capa))))
        (format t "~%")))))

