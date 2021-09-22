(defpackage :day15
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings)
  (:export main))

(in-package :day15)

(defparameter *regex* "^Disc #\\d has (\\d+) positions\\; at time=0\\, it is at position (\\d+)\\.$")

(defun decode (line)
  (multiple-value-bind (match reqs) (scan-to-strings *regex* line)
    (mapcar #'parse-integer (coerce reqs 'list))))

(defun initialize-disks (lst n)
  (when lst
    (destructuring-bind (size pos) (car lst)
      (cons
        (list size (mod (+ n pos) size))
        (initialize-disks (cdr lst) (1+ n))))))

(defun update-disks (lst)
  (when lst
    (destructuring-bind (size pos) (car lst)
      (cons
        (list size (mod (1+ pos) size))
        (update-disks (cdr lst))))))

(defun all-zero-p (lst)
  (cond
    ((null lst) t)
    ((zerop (cadar lst))
     (all-zero-p (cdr lst)))))

(defun search-delay (disks time)
  (if (all-zero-p disks)
    time
    (search-delay (update-disks disks) (1+ time))))

(defun main ()
  (let*
    ((disks (read-input-as-list 15 #'decode))
     (initialized-disks (initialize-disks disks 1)))
    (print (search-delay initialized-disks 0))))

