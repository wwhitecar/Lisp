;;William Trevor Whitecar - CS3210 - Fall 2017
;;============================================
;;Count Element Function
;;   Will count the amount of elements that are in a list
;; Parameters:
;;   lst - list to have the elements counted
;; Assumptions:
;;   1. No nested lists

(defun count-elements (lst)
  (cond ((null lst) 0)
	(t (+ 1 (count-elements (cdr lst))))
))

;;Test plan for count-elements
;;Category                    data                    result
;;==========================================================
;;Empty                       ()                         0
;;dont start at 1          (7 8 9 10)                    4
;;Count to eight           (1 2 3 5 4 6 7 8)             8
;;Desending order           (5 4 3 2 1)                  5
(setf ce-data0 '())
(setf ce-data1 '(7 8 9 10))
(setf ce-data2 '(1 2 3 4 5 6  7 8))
(setf ce-data3 '(5 4 3 2 1))

(count-elements ce-data0)
(count-elements ce-data1)
(count-elements ce-data2)
(count-elements ce-data3)

;; Count-Inveersion Function:
;;   Will count the amount of inversions contained in a list
;;   of numbers and returns it.
;; Parameters:
;;   lst - list to count inversions
;; Assumptions:
;;   1. The list contains no duplications
;;   2. No neseted lists
(defun count-inversion (lst)
  (cond ((null (cdr lst))  0)
	((> (car lst) (car (cdr lst)))  (+ 1 (count-inversion (cdr lst))))
	(t (count-inversion (cdr lst)))
))

;;Test plan for count-inversion
;;Category                    data                    result
;;==========================================================
;;Empty                       ()                         0
;;no inversions           (7 8 9 10)                     0
;;one inversion           (1 2 3 5 4 6 7 8)              1
;;Multiple inversions     (1 3 2 5 4 6 8 7)              3
;;All inversion           (5 4 3 2 1)                    4
(setf ci-data0 '(0))
(setf ci-data1 '(7 8 9 10))
(setf ci-data2 '(1 2 3 5 4 6 7 8))
(setf ci-data3 '(1 3 2 5 4 6 8 7))
(setf ci-data4 '(5 4 3 2 1))

;(count-inversion '(ci-data0))
;(count-inversion '(ci-data1))
;(count-inversion '(ci-data2))
;(count-inversion '(ci-data3))
;(count-inversion '(ci-data4))

;; Is-sorted Function:
;;   Checks to see if a list is full sorted or not.
;;   Returns true if it is; false if not
;; Parameters:
;;   lst - list to check if sorted
;; Assumptions:
;;   1. List contains no dulpicates
;;   2. No nested lists

(defun is-sorted (lst)
  (cond ((null (cdr lst)) t)
	((>= (car lst) (car (cdr lst))) nil)
	(t   (is-sorted (cdr lst)))
))

;;Test plan for is-sorted
;;Category                    data                    result
;;==========================================================
;;Empty                       ()                         t
;;Singlton                    (6)                        t
;;not sorted                 (1 3 2 4)                  nil
;;sorted                     (1 2 3 4)                   t
;;many out of order          (2 3 1 4 5 7 6 9 8)        nil
(setf datas0 '())
(setf datas1 '(6))
(setf datas2 '(1 3 2 4))
(setf datas3 '(1 2 3 4))
(setf datas4 '(2 3 1 4 5 7 6 9 8))

;(is-sorted datas0)
;(is-sorted datas1)
;(is-sorted datas2)
;(is-sorted datas3) 
;(is-sorted datas4)

;; Is-almost-sorted Function:
;;   A function that receives a list of numbers and returns
;;   true or nil for herther the list is almost sorted in
;;   ascending order. An almost sorted list is defined as
;;   one in which the number of inversions is less than 
;;   0.2n (n being the number of elements in the list).
;;   An inversion is a pair of adjacent values in the wrong
;;   order. Completely soreted lists are considered to not 
;;   be almost sorted.
;; Parameters:
;;   lst - a list of numbers
;; Assumptions:
;;   1. The list contains no duplications
;;   2. No nestes lists

(defun is-almost-sorted (lst)
  (cond ((is-sorted lst) nil)
	((null lst) nil)
	((>= (count-inversion lst) (* .2 (count-elements lst)))  nil)
	(t    t)
))
;;Test plan for count-inversion
;;Category                    data                    result
;;==========================================================
;;Empty                        ()                        nil
;;Singleton                    (10)                      nil
;;sorted                      (1 2 3 4)                  nil
;;one inversion             (1 2 4 3 5)                  nil
;;one inversion            (1 2 4 3 5 6)                  t
;;two inversions       (1 2 7 4 9 1 13 15 19 20 33)       t
;;three inversion      (1 -7 3 4 5 17 16 0 22 33)        nil
(setf data0 '())
(setf data1 '(10))
(setf data2 '(1 2 3 4))
(setf data3 '(1 2 4 3 5))
(setf data4 '(1 2 4 3 5 6))
(setf data5 '(1 2 7 4 9 1 13 15 19 20 33))
(setf data6 '(1 -7 3 4 5 17 16 0 22 33))
;(is-almost-sorted data0)
;(is-almost-sorted data1)
;(is-almost-sorted data2)
;(is-almost-sorted data3)
;(is-almost-sorted data4)
;(is-almost-sorted data5)
;(is-almost-sorted data6)
