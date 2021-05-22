(defpackage :day07
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :all-matches-as-strings :scan :split)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day07)

(defun decode (line)
  (cons
    (split "\\[\\w*\\]" line)
    (all-matches-as-strings "\\[\\w*\\]" line)))

(defun contains-abba-p (str)
  (nlet rec ((lst (coerce str 'list)))
    (destructuring-bind (a b c d &rest rst) lst
      (cond
        ((and (not (char= a b)) (char= a d) (char= b c)) t)
        ((null rst) nil)
        (t (rec (cdr lst)))))))

(defun support-tls-p (address)
  (and
    ; one supernet sequence must contains an ABBA
    (nlet rec ((lst (car address)))
      (cond
        ((null lst) nil)
        ((contains-abba-p (car lst)) t)
        (t (rec (cdr lst)))))
    ; no hypernet sequence should contains an ABBA
    (nlet rec ((lst (cdr address)))
      (cond
        ((null lst) t)
        ((contains-abba-p (car lst)) nil)
        (t (rec (cdr lst)))))))

(defun add-babs-to-list (babs str)
  (nlet rec ((lst (coerce str 'list)) (babs babs))
    (if (< (length lst) 3)
      babs
      (rec
        (cdr lst)
        (destructuring-bind (a b c &rest rst) lst
          (if (and (not (char= a b)) (char= a c))
            (cons (coerce (list b a b) 'string) babs)
            babs))))))

(defun get-babs-regex (address)
  (let
    ((babs (reduce #'add-babs-to-list (car address) :initial-value nil)))
    (unless (null babs)
      (nlet rec ((lst babs) (str ""))
        (if (null lst)
          str
          (rec
            (cdr lst) 
            (if (zerop (length str))
              (car lst)
              (format nil "~a|~a" str (car lst)))))))))

(defun support-ssl-p (address)
  (let ((regex (get-babs-regex address)))
    (when regex
      (nlet rec ((lst (cdr address)))
        (unless (null lst)
          (if (scan regex (car lst))
            t
            (rec (cdr lst))))))))

(defun main ()
  (let
    ((input (read-input-as-list 07 #'decode)))
    (print (count-valid #'support-tls-p input))
    (print (count-valid #'support-ssl-p input))))

