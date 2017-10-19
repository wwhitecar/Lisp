(defun merger(lst_one lst_two)
  (cond ((and (null lst_one) (null lst_two)) ())
	((null lst_one) lst_two)
	((null lst_two) lst_one)
	((< (car lst_one) (car lst_two)) (cons (car lst_one) (merger (cdr lst_one) lst_two)))
	(T (cons (car lst_two) (merger lst_one (cdr lst_two))))))

(print (merger '(1 3 5 7) '(2 4 6 8)))
