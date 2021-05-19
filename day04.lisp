(defpackage :day04
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :regex-replace-all :scan :scan-to-strings)
  (:import-from :fset :convert :empty-map :lookup :with)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:export main))

(in-package :day04)

(defun decode (line)
  (multiple-value-bind (_ matches) (scan-to-strings "^(.+)-(\\d+)\\[(.+)\\]$" line)
    (list
      (coerce (regex-replace-all "-" (aref matches 0) " ") 'list)
      (parse-integer (aref matches 1))
      (aref matches 2))))

(defun compare-pairs (p1 p2)
  (match (cons p1 p2)
    ((cons (cons l1 n1) (cons l2 n2))
     (if (> n1 n2)
       t
       (unless (< n1 n2)
         (char< l1 l2))))))

(defun real-room-p (room)
  (destructuring-bind (name id checksum) room
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
            (if (char= #\Space l)
              map
              (with map l (1+ (lookup map l))))))))))

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
                  (#\Space #\Space)
                  (otherwise
                    (code-char (+ (char-code #\a) (mod (+ shift-length (- (char-code l) (char-code #\a))) 26)))))
                shifted-name)))))))

(defun find-north-pole-storage-id (rooms)
  (destructuring-bind (name id checksum) (car rooms)
    (if (scan "north" (shifted-name name id))
      id
      (find-north-pole-storage-id (cdr rooms)))))

(defun main ()
  (let*
    ((rooms (read-input-as-list 04 #'decode))
     (real-rooms (remove-if-not #'real-room-p rooms)))
    (print (reduce (lambda (s r) (+ s (second r))) real-rooms :initial-value 0))
    (print (find-north-pole-storage-id real-rooms))))

