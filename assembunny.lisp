(defpackage :assembunny
  (:use :cl :aoc-misc)
  (:export :decode-assembunny :run-assembunny)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match))

(in-package :assembunny)

(defvar *regex* "^(\\w+) (\\S*)( (.+))?$")

(defun decode-param (param)
  (when param
    (let ((first-char (char param 0)))
      (if (alpha-char-p first-char)
        (cons :register (- (char-code first-char) (char-code #\a)))
        (cons :value (parse-integer param))))))

(defun decode-assembunny (line)
  (multiple-value-bind (match reqs) (scan-to-strings *regex* line)
    (match reqs
      ((vector a b _ c)
       (list 
         (intern a "KEYWORD")
         (decode-param b)
         (decode-param c))))))

(defun get-value (v regs)
  (match v
    ((cons :register x) (aref regs x))
    ((cons :value x) x)))

(defun run-assembunny (prg ignition)
  (let 
    ((regs (make-array 4 :initial-element 0))
     (len (length prg)))
    (setf (aref regs 2) ignition)
    (nlet rec ((ptr 0))
      (if (>= ptr len)
        (aref regs 0)
        (match (aref prg ptr)
          ((list :|cpy| x (cons :register r))
           (setf (aref regs r) (get-value x regs))
           (rec (1+ ptr)))
          ((list :|inc| (cons :register r) nil)
           (incf (aref regs r))
           (rec (1+ ptr)))
          ((list :|dec| (cons :register r) nil)
           (decf (aref regs r))
           (rec (1+ ptr)))
          ((list :|jnz| x y)
           (if (zerop (get-value x regs))
             (rec (1+ ptr))
             (rec (+ ptr (get-value y regs))))))))))

