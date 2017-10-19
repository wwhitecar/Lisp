;;William Trevor Whitecar - CS3210 - Fall 2017
;;============================================
;; Twin Function:
;;   Recieves a list and doubles all elements
;; Parameters:
;;   lst - list that will have all elements doubled
;; Assumptions:
;;   1. The list is not nested

(defun twin (lst)

  (cond ((null lst) ())
  
	(t (append (list (car lst) (car lst)) (twin (cdr lst))))
))

;; Test plan for twin:
;; Description         data                  result
;;--------------------------------------------------------
;; Empty List         ()                              NIL
;; Singelton          (double)            (double double)
;; 3 mixed values     (dog 2 cat)   (dog dog 2 2 cat cat)
;; 3 numbers w/ dups  (3 3 4)               (3 3 3 3 4 4)
;; 3 words            (to it pit)   (to to it it pit pit)

(print (twin '()))
(print (twin '(double)))
(print (twin '(dog 2 cat)))
(print (twin '(3 3 4)))
(print (twin '(to it pit)))

;;==============================================================
;; Untwin Function:
;;   Receives a list which may have paired elements, and remove 
;;   one of each pair. Pairs are defines as adjacent equal values.
;; Parameters:
;;   lst - list to have adjacent equal values removed 
;; Assumption:
;;   1. The list is not nested
;;   2. No more then two identical elements will be adjacent

(defun untwin (lst)
  
  (cond ((null lst) ())

	((equal (car lst) (car (cdr lst))) 
	    (cons (car lst) (untwin (cdr (cdr lst)))))

	(t   (cons (car lst) (untwin (cdr lst))))
))

;; Test plan for twin:
;; Description         data                  result
;;-------------------------------------------------
;; Empty List     ()                              NIL
;; Singelton      (dog dog)                   (dog)
;; No correction  (dog flower bee)            (dog flower bee)
;; Multiple dups  (dog dog 2 cat 7 7)         (dog 2 cat 7)
;; Single dup with good word later:
;;                (flower flower bee flower)  (flower bee flower)

(print (untwin '()))
(print (untwin '(dog dog)))
(print (untwin '(dog flower bee)))
(print (untwin '(dog dog 2 cat 7 7)))
(print (untwin '(flower flower bee flower)))
