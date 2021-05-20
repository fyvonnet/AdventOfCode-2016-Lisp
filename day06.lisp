(defpackage :day06
  (:use :cl :aoc-misc)
  (:import-from :fset :convert :empty-map :lookup :with)
  (:export main))

(in-package :day06)

(defun find-messages (lst &optional (map (empty-map 0)) new-lst messages)
  (if (null lst)
    (find-messages
      new-lst
      (empty-map 0)
      nil
      (let
        ((sorted-letters
           (sort
             (convert 'list map)
             (lambda (a b) (> (cdr a) (cdr b))))))
        (list
          (cons (caar       sorted-letters ) (first messages ))
          (cons (caar (last sorted-letters)) (second messages)))))
    (let ((word (car lst)))
      (if (zerop (length word))
        (mapcar (lambda (m) (coerce (reverse m) 'string)) messages)
        (find-messages
          (cdr lst)
          (with map (car word) (1+ (lookup map (car word))))
          (cons (cdr word) new-lst) messages)))))

(defun main ()
  (dolist (m (find-messages (read-input-as-list 06 (lambda (l) (coerce l 'list)))))
    (format t "~a~%" m)))

