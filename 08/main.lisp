#!/usr/bin/sbcl --script


;; We could, for each interior tree, check to see if it's visible
;; from north, south, east, or west and if so, count it. That sounds
;; TERRIBLY expensive computationally.
;;
;; So, instead, we could traverse each row from left to right and
;; record all trees that ARE visible (as a tuple of their coordinates).
;; Then do the same from right-to-left, top-to-bottom, and 
;; bottom-to-top... and count the size of the union of all four
;; sets of tuples.


;; Given the grid, return the column at <index>. Reverse the list if <backwards>.
(defun column (grid index &optional (backwards nil))
  (let ((col (map 'list
                  (lambda (row) 
                    (parse-integer (string (elt row index)))) 
                  grid)))
    (if backwards
      (reverse col)
      col)))

;; Given the grid, return the row at <index>. Reverse the list if <backwards>.
(defun row (grid index &optional (backwards nil))
  (let ((r (map 'list 
                (lambda (ch) (parse-integer (string ch)))
                (coerce (nth index grid) 'list))))
    (if backwards
      (reverse r)
      r)))

(defun visible-tuples (trees tuple-maker &optional (tallest-tree -1) (index 0))
  (let ((tree (car trees)))
    (cond
      ((null tree) '())
      ((> tree tallest-tree)
       (cons (funcall tuple-maker index)
             (visible-tuples (cdr trees) tuple-maker tree (+ index 1))))
      (t (visible-tuples (cdr trees) tuple-maker tallest-tree (+ index 1))))))

(defun all-visible (grid row-count col-count)
  (remove-duplicates 
    ;; coordinates are like your computer monitor: (column, row)
    ;; with column increasing left to right, row increasing from top to bottom (ie top-left coords are <0, 0>)
    (append (loop for i from 0 to (- row-count 1) 
                  append (visible-tuples (row grid i) 
                                         ;; x varies over columns for fixed row i
                                         (lambda (x) `(,x ,i))))
            (loop for i from 0 to (- row-count 1) 
                  append (visible-tuples (row grid i t)
                                         ;; x varies over columns for fixed row i, but this row is reversed -- so we start with the last column
                                         (lambda (x) `(,(- (- col-count 1) x) ,i))))
            (loop for i from 0 to (- col-count 1)
                  append (visible-tuples (column grid i)
                                         ;; x varies over rows for fixed column i
                                         (lambda (x) `(,i ,x))))
            (loop for i from 0 to (- col-count 1)
                  append (visible-tuples (column grid i t)
                                         ;; x varies over rows for fixed column i, but this column is reversed -- so we start with the last row
                                         (lambda (x) `(,i ,(- (- row-count 1) x))))))
    :test #'equal))


;; standing at <index> and looking to the beginning of <list>, this returns the list of trees in your line of sight
;; (whether you can see them or not)
(defun view-from-tree (index trees)
   (cdr (reverse (subseq trees 0 (+ index 1)))))

;; for list <l> keeps counting its elements until predicate <pred> returns true
(defun count-until (l pred)
  (let ((elt (car l)))
    (if (or (null elt) (funcall pred elt))
      0
      (+ 1 (count-until (cdr l) pred)))))

(defun score-to-front (index trees)
  (count-until
    (view-from-tree index trees)
    (lambda (x) (> x (nth index trees)))))

(defun score (c column-count r row-count grid)
  (* (score-to-front r 
                     (column grid c))
     (score-to-front (- (- row-count 1) r) 
                     (reverse (column grid c)))
     (score-to-front c 
                     (row grid r))
     (score-to-front (- (- column-count 1) c)
                     (reverse (row grid r)))))

(defun high-score (grid row-count column-count)
  (apply #'max 
         (map 'list 
              (lambda (x) (score (car x) column-count (cadr x) row-count grid))
              (all-visible grid row-count column-count))))

;; The "main" function
(let* ((grid
         (loop for line = (read-line *standard-input* nil nil)
            until (null line)
            collect line)
        )
       (row-count (length grid))
       (col-count (length (elt grid 0))))

  ;;(print grid)
  ;;(print (all-visible grid row-count col-count))

  #|
  (print grid)
  (print (column grid 0))
  (print (column grid (- col-count 1)))
  |#

  #|
  (print grid)
  (print (visible-tuples 
           (column grid 0) 
           (lambda (x) `(0 ,x))))
  (print (visible-tuples 
           (column grid 0 t) 
           (lambda (x) `(0 ,(- (- row-count 1) x)))))
  |#

  (print (length (all-visible grid row-count col-count))) ;; Q1: 1698 is answer for my input
  (print (high-score grid row-count col-count)) ;; 638666 < x < 5527005
)

