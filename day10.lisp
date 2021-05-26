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
            (with graph (parse-integer bot-str)
              (cons (list (intern  low-obj-str "KEYWORD") (parse-integer  low-num-str))
                    (list (intern high-obj-str "KEYWORD") (parse-integer high-num-str))))
            queue)
          (cons
            graph
            (queue-snoc queue (cons (parse-integer dest-bot-str)
                                    (parse-integer value-str)))))))))

(defun sort-cons (cns)
  (destructuring-bind (a . b) cns
    (if (< a b) (cons a b) (cons b a))))

(defun solve-part-one (graph queue &optional (bots-content (empty-map)))
  (destructuring-bind (bot-num . value) (queue-head queue)
    (match (lookup bots-content bot-num)
      (nil (solve-part-one graph (queue-tail queue) (with bots-content bot-num value)))
      (content
        (match (sort-cons (cons content value))
          ((cons 17 61) bot-num)
          ((cons val-low val-high)
           (destructuring-bind (dest-low . dest-high) (lookup graph bot-num)
             (solve-part-one
               graph
               (reduce
                 (lambda (q d)
                   (destructuring-bind (dest-val dest-obj dest-num) d
                     (if (eq dest-obj :|bot|)
                       (queue-snoc q (cons dest-num dest-val))
                       queue)))
                 (mapcar #'cons (list val-low val-high) (list dest-low dest-high))
                 :initial-value (queue-tail queue))
               (with bots-content bot-num nil)))))))))

(defun main ()
  (destructuring-bind (graph . queue)
    (reduce 
      #'process-input (read-input-as-list 10)
      :initial-value (cons (empty-map) (empty-queue)))
    (print (solve-part-one graph queue))))
