(defpackage :day21
  (:use :cl :aoc-misc :iterate)
  (:export main)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match))

(in-package :day21)

(defun decode (line)
  (match (split " " line)
    ((list "swap" "position" x _ _ y)
     (list 'SWAPPOS (parse-integer x) (parse-integer y)))
    ((list "swap" "letter" x _ _ y)
     (list 'SWAPLET (char x 0) (char y 0)))
    ((list "rotate" "based" _ _ _ _ x)
     (list 'ROTBASED (char x 0)))
    ((list "rotate" dir x _)
     (list 'ROTLR (intern dir "KEYWORD") (parse-integer x)))
    ((list "reverse" _ x _ y)
     (list 'REVERSE (parse-integer x) (parse-integer y)))
    ((list "move" _ x _ _ y)
     (list 'MOVE (parse-integer x) (parse-integer y)))))

(defun swap-position (str x y)
  (let ((tmp (char str x)))
    (setf (char str x) (char str y))
    (setf (char str y) tmp)))

(defun swap-letter (str len x y)
  (iter (for i below len)
    (cond
      ((char= x (char str i)) (setf (char str i) y))
      ((char= y (char str i)) (setf (char str i) x)))))

(defun rotate-left (str len x)
  (when (> x 0)
    (let ((tmp (char str 0)))
      (iter (for i from 0 below (1- len))
        (setf (char str i) (char str (1+ i))))
      (setf (char str (1- len)) tmp)) 
    (rotate-left str len (1- x))))

(defun rotate-right (str len x)
  (when (> x 0)
    (let ((tmp (char str (1- len))))
      (iter (for i from (1- len) downto 1)
        (setf (char str i) (char str (1- i))))
      (setf (char str 0) tmp))
    (rotate-right str len (1- x))))

(defun revers (str x y)
  (when (< x y)
    (let ((tmp (char str x)))
      (setf (char str x) (char str y))
      (setf (char str y) tmp))
    (revers str (1+ x) (1- y))))

(defun move (str x y)
  (if (< x y)
    (let ((tmp (char str x)))
      (iter (for i from x below y)
        (setf (char str i) (char str (1+ i))))
      (setf (char str y) tmp))
    (let ((tmp (char str x)))
      (iter (for i from x downto (1+ y))
        (setf (char str i) (char str (1- i))))
      (setf (char str y) tmp))))

(defun find-letter-position (str c)
  (nlet rec ((i 0))
    (if (char= (char str i) c)
      i
      (rec (1+ i)))))

(defun scramble (str len)
  (lambda (instr)
    (match instr
      ((list 'SWAPPOS x y)      (swap-position str     x y))
      ((list 'SWAPLET x y)      (swap-letter   str len x y))
      ((list 'ROTLR :|right| x) (rotate-right  str len x  ))
      ((list 'ROTLR :|left|  x) (rotate-left   str len x  ))
      ((list 'REVERSE x y)      (revers        str     x y))
      ((list 'MOVE x y)         (move          str     x y))
      ((list 'ROTBASED x)
       (let ((index (find-letter-position str x)))
         (rotate-right str len (+ index (if (< index 4) 1 2))))))))

(defun unscramble (str len shifts)
  (lambda (instr)
    (match instr
      ((list 'SWAPPOS x y)      (swap-position str     x y))
      ((list 'SWAPLET x y)      (swap-letter   str len x y))
      ((list 'ROTLR :|right| x) (rotate-left   str len x  ))
      ((list 'ROTLR :|left|  x) (rotate-right  str len x  ))
      ((list 'REVERSE x y)      (revers        str     x y))
      ((list 'MOVE x y)         (move          str     y x))
      ((list 'ROTBASED x)
       (let ((index (find-letter-position str x)))
         (rotate-left str len (aref shifts index)))))))

(defun reverse-shift (letters)
  (lambda (l)
    (let ((str (copy-seq letters)))
      (funcall
        (scramble str (length str))
        (list 'ROTBASED l))
      (cons
        (find-letter-position str l)
        (find-letter-position str #\a)))))

(defun main ()
  (let*
    ((input (read-input-as-list 21 #'decode))
     (letters "abcdefgh")
     (shifts
       (coerce
         (mapcar 
           #'cdr
           (sort
             (mapcar (reverse-shift letters) (coerce letters 'list))
             (lambda (a b) (< (car a) (car b)))))
         'vector)))
    (mapcar (scramble letters (length letters)) input)
    (format t "~a~%" letters)
    (let ((str "fbgdceah"))
      (mapcar (unscramble str (length str) shifts) (reverse input))
      (format t "~a~%" str))))

