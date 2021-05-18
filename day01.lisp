(defpackage :day01
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :cl-ppcre :split :scan-to-strings)
  (:import-from :fset :empty-set :with :contains?)
  (:import-from :trivia :match)
  (:export main))

(in-package :day01)

(defun decode (lst str)
  (multiple-value-bind (_ matches) (scan-to-strings "^([LR])(\\d+)$" str)
    (cons
      (cons 'walk (parse-integer (aref matches 1)))
      (cons
        (cons 'turn (if (string= (aref matches 0) "L") 'left 'right))
        lst))))

(defun walk (instructions direction coord set visited-twice)
  (match (car instructions)
    (nil (list coord visited-twice))
    ((cons 'turn turn-dir)
     (walk (cdr instructions) (turn turn-dir direction) coord set visited-twice))
    ((cons 'walk 0)
     (walk (cdr instructions) direction coord set visited-twice))
    ((cons 'walk steps)
     (destructuring-bind (new-set . new-visited-twice)
       (cond 
         (visited-twice (cons nil visited-twice))
         ((contains? set coord) (cons nil coord))
         (t (cons (with set coord) nil)))
       (walk
         (cons (cons 'walk (1- steps)) (cdr instructions))
         direction
         (next-coord direction coord)
         new-set
         new-visited-twice)))))

(defun main ()
  (let
    ((input
       (reverse 
         (reduce
           #'decode
           (split ", " (car (read-input-as-list 01)))
           :initial-value nil))))
    (print
      (mapcar
        #'manhattan-distance-from-origin
        (walk input 'north *coord-origin* (empty-set) nil)))))

