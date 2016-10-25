;;Assignment 06
;;Joyce Xu

;;Problem 1
(define curry2
	(lambda (a)
		(lambda (b)
			(lambda (c)
				(a b c)))))

;;Problem 2
(define curried-compose
	(lambda (a)
		(lambda (b)
			(lambda (c)
				(a (b c))))))

;;Problem 3
(define compose
	(lambda ls1
		(if (null? ls1)
			(lambda (ls2) ls2)
			(lambda (ls2)
				[(car ls1) [(apply compose (cdr ls1)) ls2]]))))

;;Problem 4
(define make-list-c
	(lambda (n)
		(if (eq? 0 n)
			(lambda (ls) '())
			(lambda (ls)
				(cons ls ((make-list-c (- n 1)) ls))))))

;;Problem 5
(define get-list
	(lambda (ls)
		(map car ls)))

(define helper
	(lambda (n)
		(append '(lambda) (cons (get-list (cadr n)) (cddr n)))))

(define let->application
	(lambda (ls)
		(cons (helper ls) (map cadr (cadr ls)))))

;;Problem 6
(define let*->let
	(lambda (ls)
		(if (null? [cdr (cadr ls)])
			(list 'let (cadr ls) (caddr ls))
			(list 'let (list (car ls))))))

;; Problem 7
(define filter-in
	(lambda (pred? ls)
		(filter pred? ls)))

;;Problem 8
(define filter-out
	(lambda (pred? ls)
		(remp pred? ls)))

;;Problem 9
(define sort-list-of-symbols
	(lambda (los)
		(map string->symbol (sort string<? (map symbol->string los)))))

;;Problem 10
(define invert
	(lambda (ls)
		(if (null? ls)
			'()
			(append (list (list (cadar ls) (caar ls))) (invert (cdr ls))))))

;;Problem 11
(define helper1
	(lambda (pred vec n)
		(if (eq? n (vector-length vec))
			#f
			(if (pred (vector-ref vec n))
				n
				(helper1 pred vec (+ 1 n)))))) 

(define vector-index
	(lambda (pred vec)
		(helper1 pred vec 0)))

;;Problem 12
;;(define ribassoc
;	(lambda (s los v fail-value)
;		(define helper2 (los n)
;			(if(null？ los)
;				fail-value
;				(if(eq? s (car los))
;					(car n) (helper2 (cdr los) (cdr n))))))
;	(helper2 los (vector->list v)))

