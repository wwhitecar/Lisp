(defun count1 (atm lst)
  (cond ((null lst) 0)
	((listp (car lst)) (count atm (car lst)))
	((equal atm (car lst)) (+ 1  (count atm (cdr lst))))
	(t (count atm (cdr lst)))
))


(print (count1 '6 '(6 6 6 6)))



(defun count2 (lstone lsttwo)
  (cond ((null lsttwo)  0)
	((listp (car lsttwo)) (+ (compare_list lstone (car lsttwo)) (count2 lstone (cdr lsttwo))))
	(t (count2 lstone (cdr lsttwo)))
	))

(defun compare_list (lstone lsttwo)
  (cond ((and (null lstone) (null lsttwo)) 1)
	((equal (car lstone) (car lsttwo)) (compare_list (cdr lstone) (cdr lsttwo)))
	(t 0)))

(print (count2 '() '(1 2 3 (3 4 5) () (3 4 5))))


(defun removeatm (atm lst)
  (cond (( null lst) ())
	((listp (car lst)) (remove atm (car lst)))
	((equal atm (car lst)) (remove atm (cdr lst)))
	(t (cons (car lst) (removeatm atm (cdr lst))))
	))

(print (removeatm 6 '(kitty 6 dog 6 china 6)))


(defun replacewith2 (lst)
  (cond ((null lst) ())
	(t (cons (car lst) (cons (car lst) (replacewith2 (cdr lst)))))
	))

(print (replacewith2 '(1 2 3 4 5)))

(defun issorted (lst)
  (cond ((null (cdr lst)) t)
	((>= (car lst) (car (cdr lst))) nil)
	(t (issorted (cdr lst)))
	))

(print (issorted '(1 2 3 4 5 6)))
(print (issorted '(1 2 4 3 5 6)))
