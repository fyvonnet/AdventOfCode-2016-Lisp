(defpackage :day24
  (:use :cl :aoc-misc :aoc-coord)
  (:export main)
  (:import-from :alexandria :copy-array)
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

(defun add-coords (matrix maze-map current-digit coord steps)
  (lambda (data dir)
    (destructuring-bind (q . r) data
      (let*
        ((neighb-coord (next-coord dir coord))
         (neighb-value (aref-coord maze-map neighb-coord)))
        (if neighb-value
          (progn
            (setf (aref maze-map (get-y neighb-coord) (get-x neighb-coord)) nil)
            (cons
              (queue-snoc q (cons (1+ steps) neighb-coord))
              (match neighb-value
                (t r)
                (found-digit
                  (setf (aref matrix current-digit found-digit) (1+ steps))
                  (1- r)))))
          data)))))

(defun explore-maze (matrix maze-map current-digit queue remain)
  (unless (zerop remain)
    (destructuring-bind (steps . coord) (queue-head queue)
      (destructuring-bind (new-queue . new-remain)
        (reduce
          (add-coords matrix maze-map current-digit coord steps)
          *all-absolute-dirs*
          :initial-value (cons (queue-tail queue) remain))
        (explore-maze matrix maze-map current-digit new-queue new-remain)))))

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

(defun find-shortest-path (matrix paths &optional (shortest-path 9999))
  (if (null paths)
    shortest-path
    (find-shortest-path
      matrix
      (cdr paths)
      (nlet rec ((path (car paths)) (len 0))
        (if (= 1 (length path))
          (min len shortest-path)
          (rec
            (cdr path)
            (+ len (aref matrix (first path) (second path)))))))))

(defun main ()
  (let*
    ((maze-map (read-input-as-array 24 #'identity))
     (digits (process-maze-map maze-map))
     (ndigits (length digits))
     (matrix (make-array `(,ndigits ,ndigits) :initial-element 0)))

    (nlet rec ((lst digits))
      (unless (null lst)
        (destructuring-bind (i . coord) (car lst)
          (let ((maze-map-copy (copy-array maze-map)))
            (setf (aref maze-map-copy (get-y coord) (get-x coord)) nil)
            (explore-maze matrix maze-map-copy i (queue-snoc (empty-queue) (cons 0 coord)) (1- ndigits)))
          (rec (cdr lst)))))

    (let*
      ((perms (permutations (mapcar #'car (cdr digits))))
       (paths-part-1 (mapcar (lambda (p) (cons 0 p)) perms))
       (paths-part-2 (mapcar (lambda (p) (append p '(0))) paths-part-1)))
      (dolist (p `(,paths-part-1 ,paths-part-2))
        (print (find-shortest-path matrix p))))))

