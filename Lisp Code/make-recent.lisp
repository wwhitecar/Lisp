;;William Whitecar - Cs 3210 - Fall 2017
;;=======================================
;; Make-Recent Function:
;;   Takes a word and a list of words. It puts the word in the front 
;;   of the list and calls remove-from-list.
;; Parameters:
;;   wrd - word to be moved to front of list and removed
;;   list - list to be altered
;; Assumption:
;;   1. The incoming list has no duplicates

(defun make-recent (wrd lst)
  (cons wrd (remove-from-list wrd lst))
)

;; Test plan for remove-from-list:
;; Description                   Data                        expected result
;;--------------------------------------------------------------------------
;; Word in list           (cat): (a cat came in)       (cat a came in)
;; Word not in list       (boy): (cat dog tree)        (boy cat dog tree)

;; Make-Recent Function:
;;  Searchs the list for the word passed in. If found, remove
;;  it from the list and return the new list. If not found
;;  just return the list as is.
;; Parameters:
;;   wrd - word to be moved to front of list and removed
;;   list - list to be altered
;; Assumption:
;;   1. The incoming list has no duplicates

(defun remove-from-list (wrd lst)
  (cond ((null lst)   ())

	((listp (car lst)) 
	   (cons (remove-from-list wrd (car lst))
		 (remove-from-list wrd (cdr lst))))

	((and (numberp (car lst)) (= wrd (car lst)))
	   (remove-from-list wrd (cdr lst)))

	((and (atom (car lst)) (equal wrd (car lst)))
	   (remove-from-list wrd (cdr lst)))

	(t  (cons (car lst) (remove-from-list wrd (cdr lst))))
))

;; Test plan for remove-from-list:
;; Description                   Data                        expected result
;;--------------------------------------------------------------------------
;; Empty List                  (word): ()                         NIL
;; One word no match           (word): (list)                    (list)
;; One word match              (word): (word)                      NIL
;; Multi word no match    (word): (this is a sentence)      (this is a sentence)
;; Mutli word w/ match    (word): (this is a word)          (this is a)
;; Middle of list match   (word): (this word is here)       (this is here)

(print (make-recent 'cat '(a cat came in)))
(print (make-recent 'boy '(cat dog tree)))

(print (remove-from-list 'word '()))
(print (remove-from-list 'word '(list)))
(print (remove-from-list 'word '(word)))
(print (remove-from-list 'word '(this is a sentence)))
(print (remove-from-list 'word '(this is a word)))
(print (remove-from-list 'word '(this word is here)))

