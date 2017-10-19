;; William Trevor Whitecar - CS3210 - Fall 2017
;;==============================================
;; Match Function:
;;   receives an integer and a list of integers, possible 
;;   nested. Match returns a count of how many times the 
;;   first pararmeter is found in the list.
;; Parameters:
;;   number - number being counted in the list
;;   lst - list that will be traversed looking for integer
;; Assumptions:
;;   1. Parameter number can only be a number
;;   2. List contains nothing but numbers

(defun match(number lst)
  (cond ((null lst)  0)

	((listp (car lst)) 
	 (+ (match number (car lst)) (match number (cdr lst))))

	((= number (car lst)) 
	    (+ 1 (match number (cdr lst))))

	(t  (match number (cdr lst)))
))

;;Test Plan for match
;;Description                  data                      expected result
;;----------------------------------------------------------------------
;; Empty List                    (10): ()                           0
;; One element no match          (20): (15)                         0
;; One element w/ match          (1): (1)                           1
;; Same element multiples        (1): (1 1 1 1)                     4
;; Many elements no match        (23): (11 20 5 84)                 0
;; Nested no match               (10): ((1))                        0
;; Nested w/ match               (10): ((10))                       1
;; Match and no match elements   (1): (1 2 3 4 5 4 3 2 1)           2
;; Matchs and no match elements with nesting
;;         (101): (101 101 (88 123) 15 (101) ((17 82 101) (44)))    4

(print (match 10 '()))
(print (match 20 '(15)))
(print (match 1 '(1)))
(print (match 1 '(1 1 1 1)))
(print (match 23 '(11 20 5 84)))
(print (match 10 '((1))))
(print (match 10 '((10))))
(print (match 1 '(1 2 3 4 5 4 3 2 1)))
(print (match 101 '(101 101 (88 123) 15 (101) ((17 82 101) (44)))))
