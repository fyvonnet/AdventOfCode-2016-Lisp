(defpackage :day09
  (:use :cl :aoc-misc :iterate)
  (:import-from :cl-ppcre :scan-to-strings)
  (:export main))

(in-package :day09)


(defun main ()
  (let
    ((str (car (read-input-as-list 09))))
    (iterate
      (with ptr = 0)
      (with length = 0)
      (while (< ptr (length str)))
      (if (char= #\( (aref str ptr))
        (iterate
          (with ptr2 = ptr)
          (while (not (char= #\) (aref str ptr2))))
          (incf ptr2)
          (finally
            (multiple-value-bind (match regs)
              (scan-to-strings "(\\d+)x(\\d+)" (subseq str (1+ ptr) ptr2))
              (let
                ((nchar  (parse-integer (aref regs 0)))
                 (repeat (parse-integer (aref regs 1))))
                (incf length (* repeat nchar))
                (setf ptr (+ ptr2 nchar 1))))))
        (progn
          (incf length)
          (incf ptr)))
      (finally (print length)))))

