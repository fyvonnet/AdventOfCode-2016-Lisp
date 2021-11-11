(defpackage :day22
  (:use :cl :aoc-misc aoc-coord :iterate)
  (:export main)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :trivia :match))

(in-package :day22)

(defun extract-data (lines &optional (output nil) (empty-node nil) (hole-in-wall nil) (x-max 0))
  (multiple-value-bind (_ regs) (scan-to-strings "^\/dev\/grid\/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+\%$" (car lines))
    (if (null lines)
      (list output empty-node hole-in-wall x-max)
      (destructuring-bind (x y size used avail) (map 'list #'parse-integer regs)
        (let ((coord (make-coord x y)))
          (extract-data
            (cdr lines)
            (cons (list coord size used avail) output)
            (if (zerop used) coord empty-node)
            (if (> size 500) (make-coord 0 y) hole-in-wall)
            (if (> x x-max) x x-max)))))))

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

;(defun sort-by-coord (a b)
;  (destructuring-bind ((c _ _ _) . (d _ _ _)) (cons a b)
;    (let ((diff (- (get-y c) (get-y d))))
;      (cond
;        ((minusp diff) t)
;        ((zerop  diff) (< (get-x c) (get-x d)))))))

(defun main ()
  (destructuring-bind (input empty-node hole-in-wall x-max) (extract-data (cddr (read-input-as-list 22 )))
    (print (find-viable-pairs input))
    (print
      (+
        (manhattan-distance empty-node hole-in-wall)
        (manhattan-distance hole-in-wall (make-coord (1- x-max) 0))
        (1+ (* 5 (1- x-max)))))
;    (let
;      ((limits (get-coords-limits (mapcar #'car input)))
;       (sorted-input (sort input #'sort-by-coord)))
;      (terpri)
;      (iterate
;        (for y from 0 to (getf limits :y-max))
;        (iterate
;          (for x from 0 to (getf limits :x-max))
;          (destructuring-bind (_ capa used _) (pop sorted-input)
;            (if (> capa 500)
;              (format t " XXXXX")
;              (format t " ~2a/~2a" used capa))))
;        (format t "~%")))
    ))

