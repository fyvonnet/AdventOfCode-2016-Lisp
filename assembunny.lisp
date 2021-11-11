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

(defun run-assembunny (prg &optional (change-reg nil))
  (let 
    ((regs (make-array 4 :initial-element 0))
     (len (length prg)))
    (match change-reg ((cons reg val) (setf (aref regs reg) val)))
    (nlet rec ((ptr 0))
      (if (or (< ptr 0) (>= ptr len))
        (aref regs 0)
        (progn
          ;(format t "~a / ~a~%" ptr (aref prg ptr))
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
               (rec (+ ptr (get-value y regs)))))
            ((list :|tgl| (cons :register r) nil)
             (let ((prg-ptr (+ (aref regs r) ptr)))
               (unless (or (< prg-ptr 0) (>= prg-ptr len))
                 (let ((prg-line (aref prg prg-ptr)))
                   (match prg-line
                     ((list instr a nil)
                      (if (eq instr :|inc|)
                        (setf (aref prg prg-ptr) (list :|dec| a nil))
                        (setf (aref prg prg-ptr) (list :|inc| a nil))))
                     ((list instr a b)
                      (if (eq instr :|jnz|)
                        (setf (aref prg prg-ptr) (list :|cpy| a b))
                        (setf (aref prg prg-ptr) (list :|jnz| a b))))))))
             (rec (1+ ptr)))
            (x (error (format nil "Can't execute ~a" x)))))))))

