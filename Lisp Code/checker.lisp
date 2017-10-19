;;William Trevor Whitecar - Cs3210 - Fall 2017
;;============================================
;; Check-length Function
;;   If the length of the list is anything other then 3 
;;   it will return true; false if 3.
;; Parameters:
;;   lst - list we are checking the length of
;; Assumptions:
;;

(defun check-length (lst)
  (cond ((or (> 3 (length lst)) (< 3 (length lst))) t)	
	(t  nil)
))

;;Test plan for check-length
;;Description              data                   result
;;------------------------------------------------------
;;Empty                    ()                      t
;;Singelton                (8)                     t
;;two elements             (1 2)                   t
;;Three elements           (1 2 3)                 nil
;;Four Elements            (1 2 3 4)               t
(setf cl-data0 '())
(setf cl-data1 '(8))
(setf cl-data2 '(1 2))      
(setf cl-data3 '(1 2 3))
(setf cl-data4 '(1 2 3 4))

;(check-length cl-data0))
;(check-length cl-data1))
;(check-length cl-data2))
;(check-length cl-data3))
;(check-length cl-data4))

;;-----------------------------------------------------------
;; Check-Operator Function:
;;   Checks that a the correct operator keyword is used.
;;   returns nil if and only iff plus, minus, times, divideby
;;   is passed in. True if else.
;; Parameters:
;;  wrd - the word we are checking for the correct keywork
;; Assumptions:
;;

(defun check-operator (wrd)
  (cond ((or (equal wrd 'plus) 
	     (or (equal wrd 'minus) (
                  or (equal wrd 'times) (equal wrd'divideby)))) nil)
	(t t)
))

;;Test plan for check-operator
;;Description              data              result
;;--------------------------------------------------
;;Empty                    ()                    t
;;Keyword plus             plus                 nil
;;Keyword minus            minus                nil
;;Keyword times            times                nil
;;Keywork divideby         divideby             nil
;;Incorrect keyword        notgood               t
;;Another Incorrect        nope                  t
(setf co-data0 '())
(setf co-data1 'plus)
(setf co-data2 'minus)
(setf co-data3 'times)
(setf co-data4 'divideby)
(setf co-data5 'notgood)
(setf co-data6 'nope)

;(check-operator co-data0)); t
;(check-operator co-data1)); nil
;(check-operator co-data2)); nil
;(check-operator co-data3)); nil
;(check-operator co-data4)); nil
;(check-operator co-data5)); t
;(check-operator co-data6)); t

;;---------------------------------------------------
;; Check-numeric Function:
;;    Assures that the value passed in is indeed a number. 
;;    The Value can come from a nested list if it is in the 
;;    proper formate that checker requires. Nil if proper formating
;;    or number found. True if else.
;; Parameter:
;;   wrd - checks that the number is numeric
;; Assumptions
;;   1. If it is a nested list it will only accept if it follows
;;      (number operand number) formate. Example: (7 minus 3) accepted,
;;       (3) rejected.

(defun check-numeric (lst)
  (cond ((and (listp lst) (checker lst)) nil)
	((numberp lst) nil)
        (t t)
))

;;Test plan for Check-numeric
;;Description               data                   result
;;-------------------------------------------------------
;;Empty                       ()                      t
;;Singelton                    7                     nil
;;Not number                 nope                     t
;;Nested proper              (7 plus 11)             nil
;;Nested improper number     (3)                      t
;;nested improper list       (seven plus eleven)      t

(setf cn-data0 '())
(setf cn-data1 '7)
(setf cn-data2 'nope)
(setf cn-data3 '(7 plus 11))
(setf cn-data4 '(3))
(setf cn-data5 '(seven plus elven))

;;-----------------------------------------------------------       
;; Checker Function:
;;   Expression syntax checker. A function which checks the
;;   syntax of a list of expressions with numeric operands
;;   and binary infix operators: plus, minus, times, dividesby.
;;   The checker returns nil if any of the following three error
;;   conditions are found, true otherwise: wrong number of tokens 
;;   in an expression, operands not numeric, invalid operator.
;;   If the list contains more then one error, the function terminates
;;   and reports on only the first error.
;; Parameters:
;;   lst - the list we are checking for the three specific errors
;; Assumptions
;;   1.  If a numeric value is a nested list it will only accept if it follows
;;      (number operand number) formate. Example: (7 minus 3) accepted,
;;       (3) rejected.

(defun checker (lst)
  (cond ((check-length lst) nil)
        ((check-numeric (car lst))  nil)
	((check-numeric (car (cdr (cdr lst)))) nil)
	((check-operator (car (cdr lst))) nil)
	(t t)
))

;;Test plan for checker
;;Description                data                      result
;;-----------------------------------------------------------
;;Empty                  ()                               nil
;;Correct single         (7 plus 11)                        t
;;correct nesting        (25 minus (17 times 12))           t
;;correct double nesting ((100 plus 200) minus (17 times 5))
;;                                                          t
;;Not enough args        (-4 plus)                        nil
;;Operands not numeric   ((-22 plus) (cat minus dog))     nil
;;Invalid nested operand  ((7 + 3) minus 12)              nil
;;nested operands not numeric
;;                       (-4 plus (cat minus dog))        nil

(setf data0 '())
(setf data1 '(7 plus 11))
(setf data2 '(25 minus (17 times 12)))
(setf data3 '((100 plus 200) minus (17 times 5)))
(setf data4 '(-4 plus))
(setf data5 '((-22 plus) (cat minus dog)))
(setf data6 '((7 + 3) minus 12))
(setf data7 '(-4 plus (cat minus dog)))


;(checker data0))
;(checker data1))
;(checker data2)) 
;(checker data3))
;(checker data4))
;(checker data5))
;(checker data6))
;(checker data7))

;(check-numeric cn-data0))
;(check-numeric cn-data1))
;(check-numeric cn-data2))
;(check-numeric cn-data3))
;(check-numeric cn-data4))
;(check-numeric cn-data5))
