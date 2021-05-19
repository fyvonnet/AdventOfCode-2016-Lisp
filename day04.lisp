(defpackage :day04
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :regex-replace-all :scan :scan-to-strings)
  (:import-from :fset :convert :empty-map :lookup :with)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:export main))

(in-package :day04)

(defun decode (line)
  (multiple-value-bind (match regs) (scan-to-strings "^(.+)-(\\d+)\\[(.+)\\]$" line)
    (declare (ignorable match))
    (list
      (coerce (aref regs 0) 'list)
      (parse-integer (aref regs 1))
      (aref regs 2))))

(defun compare-pairs (p1 p2)
  (match (cons p1 p2)
    ((cons (cons l1 n1) (cons l2 n2))
     (if (> n1 n2)
       t
       (unless (< n1 n2)
         (char< l1 l2))))))

(defun real-room-p (name checksum)
    (nlet rec ((lst name) (map (empty-map 0)))
      (if (null lst)
        (string=
          checksum
          (coerce
            (mapcar
              #'car
              (subseq (sort (convert 'list map) #'compare-pairs) 0 5))
            'string))
        (let ((l (car lst)))
          (rec
            (cdr lst)
            (if (char= #\- l)
              map
              (with map l (1+ (lookup map l)))))))))

(defun shifted-name (name id)
    (let*
      ((shift-length (mod id 26)))
      (nlet rec ((name name) (shifted-name nil))
        (if (null name)
          (coerce (reverse shifted-name) 'string)
          (let ((l (car name)))
            (rec
              (cdr name)
              (cons
                (case l
                  (#\- #\-)
                  (otherwise
                    (code-char (+ (char-code #\a) (mod (+ shift-length (- (char-code l) (char-code #\a))) 26)))))
                shifted-name)))))))

(defun find-answers (rooms &optional (sum 0) (storage-id nil))
  (if (null rooms)
    (list sum storage-id)
    (destructuring-bind (name id checksum) (car rooms)
      (destructuring-bind (new-sum . new-storage-id)
        (if (real-room-p name checksum)
          (cons
            (+ sum id)
            (if (and storage-id (not (scan "north" (shifted-name name id))))
              storage-id
              id))
          (cons sum storage-id))
        (find-answers (cdr rooms) new-sum new-storage-id)))))

(defun main ()
    (dolist (a (find-answers (read-input-as-list 04 #'decode))) (print a)))

