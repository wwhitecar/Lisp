(defun lst_counter (lst)
  (cond ((null lst) ())
	((listp (car lst)) (cons (count_sub (car lst)) (lst_counter (cdr lst))))
	(t (cons (car lst) (lst_counter (cdr lst))))
	))

(defun count_sub (lst)
  (cond ((null lst) 0)
	((listp (car lst)) (+ (count_sub (car lst)) (count_sub( cdr lst))))
	(t (+ (car lst) (count_sub (cdr lst))))
	))

(print (lst_counter '(6 7 (1 2 3))))

