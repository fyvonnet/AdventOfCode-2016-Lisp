(defpackage :day24
  (:use :cl :aoc-misc :aoc-coord)
  (:export main)
  (:import-from :fset :empty-set :contains?)
  (:import-from :functional-queue :empty-queue :queue-head :queue-tail :queue-snoc)
  (:import-from :permutations :permutations)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match))

(in-package :day24)

(defun aref-coord (maze-map coord)
  (destructuring-bind (x . y) coord
    (when 
      (and (>= x 0)
           (>= y 0)
           (< x (array-dimension maze-map 1))
           (< y (array-dimension maze-map 0)))
      (aref maze-map y x))))

(defun explore-maze (matrix maze-map current-digit queue remaind visited)
  (unless (zerop remaind)
    (destructuring-bind (steps . coord) (queue-head queue)
      (let
        ((next-coords
           (remove-if-not
             (lambda (c) (and (aref-coord maze-map c) (not (contains? visited c))))
             (mapcar (lambda (d) (next-coord d coord)) *all-absolute-dirs*))))
        (explore-maze
          matrix
          maze-map
          current-digit
          (reduce
            (lambda (q c) (queue-snoc q (cons (1+ steps) c)))
            next-coords :initial-value (queue-tail queue))
          (match (aref-coord maze-map coord)
            (t remaind)
            (found-digit
              (setf (aref matrix current-digit found-digit) steps)
              (1- remaind)))
          (reduce #'fset:with next-coords :initial-value visited))))))

(defun measure-distances (matrix maze-map digits)
  (nlet rec ((lst digits))
    (unless (null lst)
      (destructuring-bind (i . coord) (car lst)
        (explore-maze matrix maze-map i (queue-snoc (empty-queue) (cons 0 coord)) (length digits) (fset:with (empty-set) coord))
        (rec (cdr lst))))))

(defun process-maze-map (maze-map)
  (let*
    ((maze-width (array-dimension maze-map 1))
     (maze-nsquares (* maze-width (array-dimension maze-map 0))))
    (nlet rec ((i 0) (digits nil))
      (if (= i maze-nsquares)
        (sort digits (lambda (a b) (< (car a) (car b))))
        (multiple-value-bind (y x) (floor i maze-width)
          (rec
            (1+ i)
            (let ((c (aref maze-map y x)))
              (cond
                ((char= c #\#)
                 (setf (aref maze-map y x) nil)
                 digits)
                ((char= c #\.)
                 (setf (aref maze-map y x) t  )
                 digits)
                ((digit-char-p c)
                 (let ((i (parse-integer (string c))))
                   (setf (aref maze-map y x) i)
                   (cons (cons (parse-integer (string c)) (make-coord x y)) digits)))
                (otherwise (error (format nil "Wrong character: ~a~%" c)))))))))))

(defun main ()
  (let*
    ((maze-map (read-input-as-array 24 #'identity))
     (digits (process-maze-map maze-map))
     (len (length digits))
     (matrix (make-array `(,len ,len) :initial-element 0)))
    (measure-distances matrix maze-map digits)
    (let
      ((dists
         (mapcar
           (lambda (p) (nlet rec ((lst p) (ln 0)) (if (= 1 (length lst)) ln (rec (cdr lst) (+ ln (aref matrix (first lst) (second lst)))))))
           (mapcar (lambda (x) (cons 0 x)) (permutations (mapcar #'car (cdr digits)))))))
      (print (reduce #'min (cdr dists) :initial-value (car dists))))))

