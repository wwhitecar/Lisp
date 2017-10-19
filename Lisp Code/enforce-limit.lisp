;;William Trevor Whitecar - CS3210 - Fall 2017
;;============================================
;; Enforce-Limit Function:
;;   A function that receives a number and a list. The list
;;   may have nested lists. The function produces a new list
;;   in which all values originally over the limit are 
;;   replaced by the limit. Nested lists remain nested.
;; Parameters:
;;   limit - the max value that can be in the adjusted list
;;   lst - list that will have its values adjusted to be <= the limit
;; Assumption:
;;   1. The limit will be an Integer

(defun enforce-limit (limit lst)
  (cond ((null lst)   ())

	((and (numberp (car lst)) (< limit (car lst)))
	   (cons limit (enforce-limit limit (cdr lst))))

	((listp (car lst))
	   (cons (enforce-limit limit (car lst)) 
		 (enforce-limit limit (cdr lst))))

	(t  (cons (car lst) (enforce-limit limit (cdr lst))))
))

;;Test plan for Enforce-Limit
;;Descripttion                   data                         result
;;-------------------------------------------------------------------
;;Empty list                     (5): ()                :       NIL
;;One number under limit         (15): (6)              :       (6)
;;One number over limit          (6): (15)              :       (6)
;;Check zero in list             (1): (0)               :       (0)
;;Check zero as limit            (0): (1)               :       (0)
;;Many over the limit            (2): (3 4 5 6 7 8)     :(2 2 2 2 2 2)
;;Many under the limit           (20): (1 2 3 4 5 6 7)  :(1 2 3 4 5 6 7)
;;Under limit with words         (10): (6 math 8 cube)  :(6 math 8 cube)
;;Over limit with words          (5): (6 math 8 cube)   :(5 math 5 cube)
;;Negative none over limit       (8): (6 2 kitty 5 -16) :(6 2 kitty 5 -16)
;;Negative and an over           (8): (1 66 2 kitty -16):(1 8 2 kitty -16)
;;Nesting none over limit        (3): ((2) 1 2 (1 (2))) :((2) 1 2 (1 (2)))
;;Nesting over limit     (33): (20 (35 9) 7 100 2 () 2) :(20 (33 9) 7 33 2 () 2)

(print (enforce-limit '5 '()))
(print (enforce-limit '15 '(6)))
(print (enforce-limit '6 '(15)))
(print (enforce-limit '1 '(0)))
(print (enforce-limit '0 '(1)))
(print (enforce-limit '2 '(3 4 5 6 7 8)))
(print (enforce-limit '20 '(1 2 3 4 5 6 7)))
(print (enforce-limit '10 '(6 math 8 cube)))
(print (enforce-limit '5 '(6 math 8 cube)))
(print (enforce-limit '8 '(6 2 kitty 5 -16)))
(print (enforce-limit '8 '(1 66 2 kitty -16)))
(print (enforce-limit '3 '((2) 1 2 (1 (2)))))
(print (enforce-limit '33 '(20 (35 9) 7 100 2 () 2)))
