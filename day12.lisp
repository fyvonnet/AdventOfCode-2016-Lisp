(defpackage :day12
  (:use :cl :aoc-misc)
  (:export main)
  (:import-from :assembunny :decode-assembunny :run-assembunny))

(in-package :day12)

(defun main ()
  (let
    ((input (coerce (read-input-as-list 12 #'decode-assembunny) 'vector)))
    (dolist (ignition '(0 1)) (print (run-assembunny input ignition)))))

