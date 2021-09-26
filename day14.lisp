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

(defun md5-string (str)
  (let ((md5-str (make-string 32)))
    (nlet rec ((i 0) (md5-lst (coerce (md5sum-string str) 'list)))
      (if (null md5-lst)
        md5-str
        (progn
          (multiple-value-bind (q r) (floor (car md5-lst) 16)
            (setf (aref md5-str     i)  (aref *hex-syms* q))
            (setf (aref md5-str (1+ i)) (aref *hex-syms* r)))
          (rec (+ 2 i) (cdr md5-lst)))))))

(defun md5-multi (salt index cnt)
  (let ((md5-str (make-string 32)))
    (nlet rec ((i cnt) (str (format nil "~a~a" salt index)))
      (if (zerop i)
        str
        (rec (1- i) (md5-string str))))))

(defmacro if-match-regex ((regex salt index cnt) (chr body-t) body-nil)
  (let ((str (gensym)))
    `(let ((,str (scan-to-strings ,regex (md5-multi ,salt ,index ,cnt))))
       (if ,str
         (let ((,chr (char ,str 0)))
           ,body-t)
         ,body-nil))))

(defun search-key (salt cnt)
  (nlet rec-main
    ((start-index 1)
     (three-queue
       (nlet rec ((i 0))
         (if-match-regex (*three-regex* salt i cnt)
           (chr (queue-snoc (empty-queue) (cons i chr)))
           (rec (1+ i)))))
     (five-queue (empty-queue))
     (last-index 0)
     (countdown 64))
    (if (zerop countdown)
      last-index
      (destructuring-bind (index . chr) (queue-head three-queue)
        (let
          ((clean-five-queue
             (nlet rec ((q five-queue))
               (if (queue-empty-p q)
                 q
                 (if (<= (car (queue-head q)) index) (rec (queue-tail q)) q))))
           (end-index (+ 1001 index)))
          (destructuring-bind (new-three-queue . complete-five-queue)
            (nlet rec ((q3 (queue-tail three-queue)) (q5 clean-five-queue) (i start-index))
              (if (= i end-index)
                (cons q3 q5)
                (if-match-regex (*three-regex* salt i cnt)
                  (chr3 
                    (let
                      ((new-q3 (queue-snoc q3 (cons i chr3)))
                       (new-q5 
                         (if-match-regex (*five-regex* salt i cnt)
                           (chr5 (queue-snoc q5 (cons i chr5)))
                           q5)))
                      (rec new-q3 new-q5 (1+ i))))
                  (nil (rec q3 q5 (1+ i))))))
            (nlet rec ((q5 complete-five-queue))
              (cond
                ((queue-empty-p q5) (rec-main end-index new-three-queue complete-five-queue 0 countdown))
                ((char= (cdr (queue-head q5)) chr)
                 (rec-main end-index new-three-queue complete-five-queue index (1- countdown)))
                (t (rec (queue-tail q5)))))))))))

(defun main ()
  (let
    ((salt (first (read-input-as-list 14))))
    (dolist (c '(1 2017)) (format t "~a~%" (search-key salt c)))))

