(defpackage :day17
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :md5-hexadecimal :md5-hexadecimal)
  (:import-from :leftist-heap :leftist-insert :leftist-find-min :leftist-delete-min)
  (:export main))

(in-package :day17)

(defun compare (a b)
  (< (car a) (car b)))

(defun populate-queue (new-len suffix coord)
  (lambda (queue data)
    (destructuring-bind (letter abs-dir check-lim dir-letter) data
      (if 
        (and
          (char>= letter #\b)
          (char<= letter #\f)
          (not (funcall check-lim coord)))
        (leftist-insert
          (list
            new-len
            (concatenate 'string suffix dir-letter)
            (next-coord abs-dir coord))
          queue #'compare)
        queue))))

(defun navigate (prefix queue)
  (destructuring-bind (len suffix coordinates) (leftist-find-min queue)
    (if (coord= coordinates (make-coord 3 3))
      suffix
      (navigate
        prefix
        (reduce
          (populate-queue (1+ len) suffix coordinates)
          (mapcar
            #'cons
            (coerce (subseq (md5-hexadecimal (concatenate 'string prefix suffix)) 0 4) 'list)
            (list
              (list 'north (lambda (c) (= (get-y c) 0)) "U")
              (list 'south (lambda (c) (= (get-y c) 3)) "D")
              (list 'west  (lambda (c) (= (get-x c) 0)) "L")
              (list 'east  (lambda (c) (= (get-x c) 3)) "R")))
          :initial-value (leftist-delete-min queue #'compare))))))

(defun main ()
  (let
    ((input (car (read-input-as-list 17)))
     (queue (leftist-insert (list 0 "" (make-coord 0 0)) nil #'compare)))
    (princ (navigate input queue))))

