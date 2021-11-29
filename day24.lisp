(defpackage :day24
  (:use :cl :aoc-misc :aoc-coord)
  (:export main)
  (:import-from :alexandria :copy-array)
  (:import-from :functional-queue :empty-queue :queue-head :queue-tail :queue-snoc)
  (:import-from :permutations :permutations)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match))

(in-package :day24)

(defun add-coords (matrix maze-map current-digit coord steps)
  (lambda (data dir)
    (destructuring-bind (q . r) data
      (let*
        ((neighb-coord (next-coord dir coord))
         (neighb-value (aref-coord maze-map neighb-coord)))
        (if neighb-value
          (progn
            (setf (aref-coord maze-map neighb-coord) nil)
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
     (digits (scan-matrix #'process-maze-map maze-map))
     (ndigits (length digits))
     (matrix (make-array `(,ndigits ,ndigits) :initial-element 0)))

    (nlet rec ((lst digits))
      (unless (null lst)
        (destructuring-bind (i . coord) (car lst)
          (let ((maze-map-copy (copy-array maze-map)))
            (setf (aref-coord maze-map-copy coord) nil)
            (explore-maze matrix maze-map-copy i (queue-snoc (empty-queue) (cons 0 coord)) (1- ndigits)))
          (rec (cdr lst)))))

    (let*
      ((perms (permutations (remove-if #'zerop (mapcar #'car digits))))
       (paths-part-1 (mapcar (lambda (p) (cons 0 p)) perms))
       (paths-part-2 (mapcar (lambda (p) (append p '(0))) paths-part-1)))
      (dolist (p `(,paths-part-1 ,paths-part-2))
        (print (find-shortest-path matrix p))))))

