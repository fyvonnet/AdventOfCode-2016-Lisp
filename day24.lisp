(defpackage :day24
  (:use :cl :aoc-misc :aoc-coord :forfuncs :leftist-heap)
  (:export main)
  (:import-from :alexandria :copy-array)
  (:import-from :functional-queue :empty-queue :queue-head :queue-tail :queue-snoc)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match))

(in-package :day24)

(defvar matrix)
(defvar all-digits)

(defun explore-maze (maze-map current-digit queue remain)
  (unless (zerop remain)
    (destructuring-bind (steps . coord) (queue-head queue)
      (for/fold
        ((new-queue (queue-tail queue))
         (new-remain remain))
        ((dir *all-absolute-dirs*))
        (let*
          ((neighb-coord (next-coord dir coord))
           (neighb-value (aref-coord maze-map neighb-coord)))
          (if neighb-value
            (progn
              (setf (aref-coord maze-map neighb-coord) nil)
              (values
                (queue-snoc new-queue (cons (1+ steps) neighb-coord))
                (match neighb-value
                  (t new-remain)
                  (found-digit
                    (setf (aref matrix current-digit found-digit) (1+ steps))
                    (1- new-remain)))))
            (values new-queue new-remain)))
        :result
        (explore-maze maze-map current-digit new-queue new-remain)))))

(defun process-maze-map (maze-map coord digits)
  (let ((c (aref-coord maze-map coord)))
    (cond
      ((char= c #\#)
       (setf (aref-coord maze-map coord) nil)
       digits)
      ((char= c #\.)
       (setf (aref-coord maze-map coord) t  )
       digits)
      ((digit-char-p c)
       (let ((i (parse-integer (string c))))
         (setf (aref-coord maze-map coord) i)
         (cons (cons i coord) digits)))
      (otherwise (error (format nil "Wrong character: ~a~%" c))))))

(defun compare (a b)
  (< (fourth a) (fourth b)))

(defun find-shortest-path (&optional (heap (leftist-insert (list 0 '(0) 0 0) nil #'compare)) (part2 nil))
  (destructuring-bind (node visited found dist) (leftist-find-min heap)
    (cond
      ((= (1- (length all-digits)) found)
       (if part2
         (find-shortest-path
           (leftist-insert
             (list 0 nil 8 (+ dist (aref matrix 0 (car visited))))
             (leftist-delete-min heap #'compare)
             #'compare) t)
         (progn
           (format t "~D~%" dist)
           (find-shortest-path heap t))))
      ((= (length all-digits) found) (format t "~D~%" dist))
      (t
        (for/fold
          ((new-heap (leftist-delete-min heap #'compare)))
          ((n (set-difference all-digits visited)))
          (leftist-insert (list n (cons n visited) (1+ found) (+ dist (aref matrix node n))) new-heap #'compare)
          :result (find-shortest-path new-heap part2))))))

(defun main ()
  (let*
    ((maze-map (read-input-as-array 24))
     (digits (scan-matrix #'process-maze-map maze-map))
     (ndigits (length digits)))

    (setf matrix (make-array `(,ndigits ,ndigits) :initial-element 0))
    (setf all-digits (loop for i below ndigits collect i))

    (loop for d in digits doing
      (destructuring-bind (i . coord) d
        (let ((maze-map-copy (copy-array maze-map)))
          (setf (aref-coord maze-map-copy coord) nil)
          (explore-maze maze-map-copy i (queue-snoc (empty-queue) (cons 0 coord)) (1- ndigits)))))

    (find-shortest-path)))

