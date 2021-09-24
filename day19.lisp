(defpackage :day19
  (:use :cl :aoc-misc)
  (:import-from :functional-queue :empty-queue :queue-empty-p :queue-head :queue-tail :queue-snoc)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day19)

(defun myfunc (queue)
  (if (queue-empty-p (queue-tail queue))
    (1+ (queue-head queue))
    (myfunc
      (queue-snoc
        (queue-tail
          (queue-tail
            queue))
        (queue-head queue)))))

(defun main ()
  (let*
    ((input (car (read-input-as-list 19 #'parse-integer)))
     (queue
       (nlet rec ((q (empty-queue)) (i 0))
         ;(if (= i 5)
         (if (= i input)
           q
           (rec (queue-snoc q i) (1+ i))))))
    (print (myfunc queue))))

