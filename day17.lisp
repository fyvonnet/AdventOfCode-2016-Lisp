(defpackage :day17
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :md5-hexadecimal :md5-hexadecimal)
  (:import-from :sb-md5 :md5sum-string)
  (:import-from :leftist-heap :leftist-insert :leftist-find-min :leftist-delete-min :leftist-empty)
  (:export main))

(in-package :day17)

(defun compare (a b)
  (< (car a) (car b)))

(defun populate-heap (new-len suffix coord)
  (lambda (heap data)
    (destructuring-bind (value abs-dir check-lim dir-letter) data
      (if 
        (and
          (>= value 11)
          (<= value 15)
          (not (funcall check-lim coord)))
        (leftist-insert
          (list
            new-len
            (concatenate 'string suffix dir-letter)
            (next-coord abs-dir coord))
          heap #'compare)
        heap))))

(defun navigate (prefix heap shortest-path max-length)
  (if (leftist-empty heap)
    (list shortest-path max-length)
    (let ((heap-remainder (leftist-delete-min heap #'compare)))
      (destructuring-bind (len suffix coordinates) (leftist-find-min heap)
        (if (coord= coordinates (make-coord 3 3))
          (navigate prefix heap-remainder (if shortest-path shortest-path suffix) len)
          (navigate
            prefix
            (reduce
              (populate-heap (1+ len) suffix coordinates)
              (mapcar
                #'cons
                (destructuring-bind (a b)
                  (coerce (subseq (md5sum-string (concatenate 'string prefix suffix)) 0 2) 'list)
                  (multiple-value-bind (q1 r1) (floor a 16)
                    (multiple-value-bind (q2 r2) (floor b 16)
                      (list q1 r1 q2 r2))))
                (list
                  (list 'north (lambda (c) (= (get-y c) 0)) "U")
                  (list 'south (lambda (c) (= (get-y c) 3)) "D")
                  (list 'west  (lambda (c) (= (get-x c) 0)) "L")
                  (list 'east  (lambda (c) (= (get-x c) 3)) "R")))
              :initial-value heap-remainder)
            shortest-path max-length))))))

(defun main ()
  (let
    ((input (car (read-input-as-list 17)))
     (heap (leftist-insert (list 0 "" (make-coord 0 0)) nil #'compare)))

    (dolist (x (navigate input heap nil nil))
      (princ x)
      (terpri))))

