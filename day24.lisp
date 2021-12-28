(defpackage :day24
  (:use :cl :aoc-misc :aoc-coord :forfuncs)
  (:export main)
  (:import-from :alexandria :copy-array)
  (:import-from :functional-queue :empty-queue :queue-head :queue-tail :queue-snoc)
  (:import-from :permutations :permutations)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match))

(in-package :day24)

(defun explore-maze (matrix maze-map current-digit queue remain)
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

(defun find-shortest-paths (matrix paths &optional (shortest-paths (list 9999 9999)))
  (if (null paths)
    shortest-paths
    (find-shortest-paths
      matrix
      (cdr paths)
      (nlet rec ((path (car paths)) (len 0))
        (if (= 1 (length path))
          (list 
            (min len (first shortest-paths))
            (min (+ len (aref matrix 0 (car path))) (second shortest-paths)))
          (rec
            (cdr path)
            (+ len (aref matrix (first path) (second path)))))))))

(defun main ()
  (let*
    ((maze-map (read-input-as-array 24 #'identity))
     (digits (scan-matrix #'process-maze-map maze-map))
     (ndigits (length digits))
     (matrix (make-array `(,ndigits ,ndigits) :initial-element 0)))

    (loop for d in digits doing
      (destructuring-bind (i . coord) d
        (let ((maze-map-copy (copy-array maze-map)))
          (setf (aref-coord maze-map-copy coord) nil)
          (explore-maze matrix maze-map-copy i (queue-snoc (empty-queue) (cons 0 coord)) (1- ndigits)))))

    (dolist
      (answer
        (find-shortest-paths
          matrix
          (mapcar
            (lambda (p) (cons 0 p))
            (permutations (remove-if #'zerop (mapcar #'car digits))))))
      (format t "~d~%" answer))))

