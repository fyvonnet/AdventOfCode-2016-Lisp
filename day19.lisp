(defpackage :day19
  (:use :cl :aoc-misc)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day19)

; https://www.youtube.com/watch?v=uCsD3ZGzMgE
(defun josephus (n &optional (l 0) (bit-value 1))
  (if (= n 1)
    (1+ (* 2 l))
    (multiple-value-bind (q r) (floor n 2)
      (josephus q (+ l (* r bit-value)) (* 2 bit-value)))))

#|
; functions for computing the first solutions to part 2 for analysis

(defun find-next (current n lst)
  (if (zerop n)
    current
    (let ((new-current (mod (1+ current) (array-dimension lst 0))))
      (find-next
        new-current
        (if (aref lst new-current) (1- n) n)
        lst))))

(defun turn (current lst cnt)
  (if (= 1 cnt)
    (1+ current)
    (progn
      (setf (aref lst (find-next current (floor cnt 2) lst)) nil)
      (turn
        (find-next current 1 lst)
        lst
        (1- cnt)))))

(defun compute-solutions (n)
  (let
    ((lst (make-array n :initial-element t)))
    (format t "~a ~a~%" n (turn 0 lst n))
    (compute-solutions (1+ n))))
|#

(defun main ()
  (let*
    ((input (car (read-input-as-list 19 #'parse-integer)))
     (lst (make-array 10 :initial-element t)))
    (print (josephus input))
    ;(compute-solutions 1)
    (print 
      (nlet rec ((n 1) (last-elf 1) (increment 1) (change-at 1))
        (cond
          ((= n input) last-elf)
          ((= n last-elf) (rec (1+ n) 1 1 n))
          ((= last-elf change-at) (rec (1+ n) (+ 2 last-elf) 2 change-at))
          (t (rec (1+ n) (+ increment last-elf) increment change-at)))))))

