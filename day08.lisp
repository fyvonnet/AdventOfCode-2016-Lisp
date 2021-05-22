(defpackage :day08
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :trivia :match)
  (:export main))

(in-package :day08)

(defconstant *screen-width*  50)
(defconstant *screen-height*  6)

(defun decode (line)
  (multiple-value-bind (match reqs)
    (scan-to-strings "rect (\\d+)x(\\d+)|rotate row y=(\\d+) by (\\d+)|rotate column x=(\\d+) by (\\d+)" line)
    (declare (ignorable match))
    (cond
      ((aref reqs 0)
       (cons
         'rect
         (mapcar (lambda (n) (parse-integer (aref reqs n))) '(0 1))))
      ((aref reqs 2)
       (cons
         'rotate-row
         (mapcar (lambda (n) (parse-integer (aref reqs n))) '(2 3))))
      ((aref reqs 4)
       (cons
         'rotate-column
         (mapcar (lambda (n) (parse-integer (aref reqs n))) '(4 5)))))))

(defun change (arr oper)
  (match oper
    ((list 'rect a b)
     (loop
       for y below b
       do (loop for x below a do (setf (aref arr x y) t))))
    ((list 'rotate-row row pixels)
     (let ((temp-vector (make-array *screen-width*)))
       (loop
         for i below *screen-width*
         do (setf (aref temp-vector i) (aref arr i row)))
       (loop
         for i below *screen-width*
         do (setf (aref arr (mod (+ pixels i) *screen-width*) row) (aref temp-vector i)))))
    ((list 'rotate-column column pixels)
     (let ((temp-vector (make-array *screen-height*)))
       (loop
         for i below *screen-height*
         do (setf (aref temp-vector i) (aref arr column i)))
       (loop
         for i below *screen-height*
         do (setf (aref arr column (mod (+ pixels i) *screen-height*)) (aref temp-vector i)))))))


(defun main ()
  (let
    ((input (read-input-as-list 08 #'decode))
     (arr (make-array (list *screen-width* *screen-height*) :initial-element nil)))
    (loop
      for command in input
      do (change arr command))
  (loop
    with count = 0
    for y below *screen-height*
    do (loop
         for x below *screen-width*
         do (format t (if (aref arr x y) (progn (incf count) "#") " "))
         finally (terpri))
    finally (print count))))

