(defpackage :day03
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :split)
  (:export main))

(in-package :day03)

(defun decode (line)
    (mapcar
      #'parse-integer
      (cdr (split "\\s+" line))))

(defun possible-triangle-p (triangle)
  (destructuring-bind (a b c) (sort triangle #'<)
    (> (+ a b) c)))

(defun rotate (lsts)
  (unless (null (car lsts))
    (cons
      (mapcar #'car lsts)
      (rotate (mapcar #'cdr lsts)))))

(defun reorganize (triangles)
  (unless (null triangles)
    (destructuring-bind (a b c &rest rst) triangles
      (append
        (rotate (list a b c))
        (reorganize rst)))))

(defun main ()
  (let*
    ((triangles (read-input-as-list 03 #'decode))
     (reorganized-triangles (reorganize triangles)))
    (dolist (tr (list triangles reorganized-triangles))
      (print (count-valid #'possible-triangle-p tr)))))
