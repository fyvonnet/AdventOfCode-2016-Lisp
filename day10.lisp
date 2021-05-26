(defpackage :day10
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :fset :empty-map :lookup :with)
  (:import-from :functional-queue :empty-queue :queue-head :queue-snoc :queue-tail)
  (:import-from :trivia :match)
  (:export main))

(in-package :day10)

(defvar *regex* "bot (\\d+) gives low to (\\w+) (\\d+) and high to (\\w+) (\\d+)|value (\\d+) goes to bot (\\d+)")

(defun process-input (data line)
  (destructuring-bind (graph . queue) data
    (multiple-value-bind (match reqs) (scan-to-strings *regex* line)
      (destructuring-bind (bot-str low-obj-str low-num-str high-obj-str high-num-str value-str dest-bot-str)
        (coerce reqs 'list)
        (if bot-str
          (cons
            (with graph (cons :|bot| (parse-integer bot-str))
              (cons (cons (intern  low-obj-str "KEYWORD") (parse-integer  low-num-str))
                    (cons (intern high-obj-str "KEYWORD") (parse-integer high-num-str))))
            queue)
          (cons
            graph
            (queue-snoc queue (cons (cons :|bot| (parse-integer dest-bot-str))
                                    (parse-integer value-str)))))))))

(defun sort-cons (cns)
  (destructuring-bind (a . b) cns
    (if (< a b) (cons a b) (cons b a))))

(defun solve (graph queue &optional (objects-content (empty-map)))
  (destructuring-bind (a b c) (mapcar (lambda (n) (lookup objects-content (cons :|output| n))) '(0 1 2))
    (if (and a b c)
      (print (* a b c))
      (destructuring-bind (dest-id . value) (queue-head queue)
        (match (lookup objects-content dest-id)
          (nil (solve graph (queue-tail queue) (with objects-content dest-id value)))
          (content
            (match (sort-cons (cons content value))
              ((cons val-low val-high)
               (when (and (= val-low 17) (= val-high 61)) (print (cdr dest-id)))
               (destructuring-bind (dest-low . dest-high) (lookup graph dest-id)
                 (solve
                   graph
                   (reduce
                     (lambda (q d)
                       (destructuring-bind (dest-id . dest-val) d
                         (queue-snoc q (cons dest-id dest-val))))
                     (mapcar #'cons (list dest-low dest-high) (list val-low val-high))
                     :initial-value (queue-tail queue))
                   (with objects-content dest-id nil)))))))))))

(defun main ()
  (destructuring-bind (graph . queue)
    (reduce 
      #'process-input (read-input-as-list 10)
      :initial-value (cons (empty-map) (empty-queue)))
    (solve graph queue)))

