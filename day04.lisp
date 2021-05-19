(defpackage :day04
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :regex-replace-all :scan-to-strings)
  (:import-from :fset :convert :empty-map :lookup :with)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:export main))

(in-package :day04)

(defun decode (line)
  (multiple-value-bind (_ matches) (scan-to-strings "^(.+)-(\\d+)\\[(.+)\\]$" line)
    (list
      (coerce (regex-replace-all "-" (aref matches 0) "") 'list)
      (parse-integer (aref matches 1))
      (aref matches 2))))

(defun compare-pairs (p1 p2)
  (match (cons p1 p2)
    ((cons (cons l1 n1) (cons l2 n2))
     (if (> n1 n2)
       t
       (unless (< n1 n2)
         (char< l1 l2))))))

(defun sum-real-rooms (sum room)
  (destructuring-bind (name id checksum) room
    (nlet rec ((lst name) (map (empty-map 0)))
      (if (null lst)
        (if
          (string=
            checksum
            (coerce
              (mapcar
                #'car
                (subseq (sort (convert 'list map) #'compare-pairs) 0 5))
              'string))
          (+ sum id)
          sum)
        (rec (cdr lst) (with map (car lst) (1+ (lookup map (car lst)))))))))

(defun main ()
  (let
    ((input (read-input-as-list 04 #'decode)))
    (print (reduce #'sum-real-rooms input :initial-value 0))))
