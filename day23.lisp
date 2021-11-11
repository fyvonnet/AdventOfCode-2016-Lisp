(defpackage :day23
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :assembunny :decode-assembunny :run-assembunny))

(in-package :day23)

(defun main ()
  (let
    ((input (coerce (read-input-as-list 23 #'decode-assembunny) 'vector)))
    (print (run-assembunny input (cons 0 7)))))

