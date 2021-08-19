(defpackage :day13
  (:use :cl :aoc-misc :aoc-coord)
  (:export main)
  (:import-from :fset :empty-set :contains? :with :size)
  (:import-from :trivia :match)
  (:import-from :serapeum :nlet)
  (:import-from :functional-queue :empty-queue :queue-snoc :queue-head :queue-tail))

(in-package :day13)


(defparameter *dest-coord* (make-coord 31 39))

(defun count-bits (num)
  (if (zerop num)
    0
    (multiple-value-bind (q r) (floor num 2)
      (+ r (count-bits q)))))

(defun try-move (coord direction fav-num visited)
  (let*
    ((new-coord (next-coord direction coord))
     (x (get-x new-coord))
     (y (get-y new-coord)))
    (when
      (and
        (not (minusp x))
        (not (minusp y))
        (not (contains? visited new-coord))
        (zerop
          (rem
            (count-bits
              (+
                fav-num
                (* x x)
                (* 3 x)
                (* 2 x y)
                y
                (* y y))) 
            2)))
      new-coord)))

(defun add-to-queue (fav-num visited coord steps)
  (lambda (queue direction)
    (match (try-move coord direction fav-num visited)
      (nil queue)
      (new-coord (queue-snoc queue (cons new-coord steps))))))

(defun main ()
  (let
    ((fav-num (parse-integer (car (read-input-as-list 13)))))
    (nlet rec ((queue (queue-snoc (empty-queue) (cons (make-coord 1 1) 0))) (visited (empty-set)) (empty-squares nil))
      (match (queue-head queue)
        (nil (error "empty queue"))
        ((cons coord steps)
         (if (coord= coord *dest-coord*)
           (format t "~a~%~a~%" steps empty-squares)
           (rec
             (reduce
               (add-to-queue fav-num visited coord (1+ steps))
               *all-absolute-dirs*
               :initial-value (queue-tail queue))
             (with visited coord)
             (if (and (> steps 50) (null empty-squares))
               (size visited)
               empty-squares))))))))

