(defpackage :day14
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :functional-queue :empty-queue :queue-snoc :queue-head :queue-tail :queue-empty-p)
  (:import-from :sb-md5 :md5sum-string)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:export main))

(in-package :day14)

(defparameter *hex-syms* "0123456789abcdef")
(defparameter *three-regex* (format nil "(([~a])\\2\\2)"       *hex-syms*))
(defparameter *five-regex*  (format nil "(([~a])\\2\\2\\2\\2)" *hex-syms*))

(defun md5-convert (salt index)
  (let ((md5-str (make-string 32)))
    (nlet rec ((i 0) (md5-lst (coerce (md5sum-string (format nil "~a~a" salt index)) 'list)))
      (if (null md5-lst)
        md5-str
        (progn
          (multiple-value-bind (q r) (floor (car md5-lst) 16)
            (setf (aref md5-str     i)  (aref *hex-syms* q))
            (setf (aref md5-str (1+ i)) (aref *hex-syms* r)))
          (rec (+ 2 i) (cdr md5-lst)))))))

(defun check-regex (regex salt index)
  (match (scan-to-strings regex (md5-convert salt index))
    (nil nil)
    (str (char str 0))))

(defun find-next-three (salt index)
  (match (scan-to-strings *three-regex* (md5-convert "abc" i))
    (nil (find-next-three salt (1+ index)))
    (str (queue-snoc (empty-queue) (cons i (char str 0))))))

(defun search-key (start-index three-queue five-queue salt last-index countdown)
  (if (zerop countdown)
    last-index
    (destructuring-bind (index . chr) (queue-head three-queue)
      (let
        ((clean-five-queue
           (nlet rec ((q five-queue))
             (if (queue-empty-p q)
               q
               (destructuring-bind (i . _) (queue-head q)
                 (if (<= i index) (rec (queue-tail q)) q)))))
         (end-index (+ 1001 index)))
        (destructuring-bind (new-three-queue . complete-five-queue)
          (nlet rec ((q3 (queue-tail three-queue)) (q5 clean-five-queue) (i start-index))
            (if (= i end-index)
              (cons q3 q5)
              (match (check-regex *three-regex* salt i)
                (nil (rec q3 q5 (1+ i)))
                (chr3 
                  (let
                    ((new-q3 (queue-snoc q3 (cons i chr3)))
                     (new-q5 
                       (match (check-regex *five-regex* salt i)
                         (nil q5)
                         (chr5 (queue-snoc q5 (cons i chr5))))))
                    (rec new-q3 new-q5 (1+ i)))))))
          (nlet rec ((q5 complete-five-queue))
            (cond
              ((queue-empty-p q5) (search-key end-index new-three-queue complete-five-queue salt 0 countdown))
              ((char= (cdr (queue-head q5)) chr)
               (search-key end-index new-three-queue complete-five-queue salt index (1- countdown)))
              (t (rec (queue-tail q5))))))))))

(defun main ()
  (let*
    ((salt (first (read-input-as-list 14)))
     (three-queue
       (nlet rec ((i 0))
         (match (check-regex *three-regex* salt i)
           (nil (rec (1+ i)))
           (chr (queue-snoc (empty-queue) (cons i chr)))))))
    (print (search-key 1 three-queue (empty-queue) salt 0 64))))
