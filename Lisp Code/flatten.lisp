;; William Trevor Whitecar - CS3210 - Fall 2017
;;=============================================
;; Flatten Function:
;;   Receives a list of arbitrary depth, and returns a list
;;   containing all the smae elements, in the same order, 
;;   but now at the top level. Note that flattening an embedded NIL
;;   removes it.
;; Parameters:
;;   lst - list that will be transformed to a flat list
;; Assumptions:
;;   N/A

(defun flatten (lst)
  (cond ((null lst)  ())

((listp (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))

	(t   (cons (car lst) (flatten (cdr lst))))
))

;;Test plan for Flatten
;;Description                data                                   result
;;-----------------------------------------------------------------------------
;;Empty List                  ()                                      NIL
;;No nesting               (day night)                              (day night)
;;3 Nested list  ((how many) (lists are) (nested))   (how many lists are nested)
;;Depth 4 nested  (one (two (three (four))))        (one two three fourth)
;;Variety       (a b (high low) () (e (f (deep) h) (a b high low NIL e f deep h)


(print (flatten '()))
(print (flatten '(day night)))
(print (flatten '((how many) (lists are) (nested))))
(print (flatten '(one (two (three (four))))))
(print (flatten '(a b (high low) () (e (f (deep) h)))))
