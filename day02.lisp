(defpackage :day02
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :fset :empty-map :lookup :with)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day02)

(defun decode (line)
  (let ((plist '(#\U north #\D south #\L west #\R east)))
    (mapcar (lambda (c) (getf plist c)) (coerce line 'list))))

(defun make-keys-map-line (keys map y &optional (x 0))
  (if (null keys)
    map
    (make-keys-map-line
      (cdr keys)
      (if (char= #\Space (car keys))
        map
        (with map (make-coord x y) (car keys)))
      y
      (1+ x))))

(defun make-keys-map (lines)
  (let ((shift (- (floor (/ (length lines) 2)))))
    (nlet rec ((lines lines) (map (empty-map)) (y shift))
      (if (null lines)
        map
        (rec
          (cdr lines)
          (make-keys-map-line (coerce (car lines) 'list) map y shift)
          (1+ y))))))

(defun move (directions coord keys-map)
  (if (null directions)
    coord
    (let ((new-coord (next-coord (car directions) coord)))
      (move
        (cdr directions)
        (if (null (lookup keys-map new-coord))
          coord
          new-coord)
        keys-map))))

(defun all-moves (moves-lst coord keys-map)
  (unless (null moves-lst)
    (let ((new-coord (move (car moves-lst) coord keys-map)))
      (cons
        (lookup keys-map new-coord)
        (all-moves (cdr moves-lst) new-coord keys-map)))))

(defun main ()
  (let
    ((input (read-input-as-list 2 #'decode))
     (keys-map 
       (make-keys-map
         (list
           "123"
           "456"
           "789")))
     (2nd-keys-map
      (make-keys-map
        (list
          "  1  "
          " 234 "
          "56789"
          " ABC "
          "  D  "))))
    (dolist (m (list keys-map 2nd-keys-map))
      (format t "~a~%" (coerce (all-moves input *coord-origin* m) 'string)))))
