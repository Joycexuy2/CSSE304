;; Assignment 7
;; Joyce Xu

;; #1
(define list-helper
	(lambda (initial n ls vec)
		(if (eq? 0 (length ls))
			vec
			[begin (vector-set! vec initial (list-ref ls n))
				(if(eq? (+ 1 n) (length ls))
					vec
					(list-helper (+ 1 initial) (+ 1 n) ls vec))])))

(define vector-helper
	(lambda (initial n old new)
		(if (eq? 0 (vector-length old))
			new
			(begin [vector-set! new initial (vector-ref old n)]
				(if(eq? (+ 1 n) (vector-length old))
					new
					(vector-helper (+ 1 initial) (+ 1 n) old new))))))

(define vector-append-list
	(lambda (vec ls)
		(list-helper (vector-length vec)
			0
			ls
			(vector-helper 0 0 vec (make-vector [+ (vector-length vec) (length ls)])))))

;; #2
(define qsort-left
	(lambda (pred ls pt)
		[if (null? ls)
			ls
			(if [pred pt (car ls)]
				(qsort-left pred (cdr ls) pt)
				(cons (car ls) (qsort-left pred (cdr ls) pt)))]))

(define qsort-right
	(lambda (pred ls pt)
		(if (null? ls)
			ls
			(if [pred pt (car ls)]
				(cons (car ls) (qsort-right pred (cdr ls) pt))
				(qsort-right pred (cdr ls) pt)))))

(define qsort
	(lambda (pred lst)
		(cond ((null? lst) lst)
			  ((null? (cdr lst)) lst)
				[else (append 
					[qsort pred (qsort-left pred (cdr lst) (car lst))]
					[list (car lst)]
					[qsort pred (qsort-right pred (cdr lst) (car lst))])])))

;; #3
;;connected?
;(define connected?
;	(lambda (g)
;		(cond
;			[(null? g) #t]
;			[(null? (cdr g)) #t]
;			[else
 ;     			(let ls ((visited (cadr (car g))) (lst g))
  ;    				)
	;		]
	;		)
	;	))
;; #4
(define reverse-it
	(lambda (lst)
		(let reverse ((lst lst) (new '()))
			(if (null? lst)
				new
				(reverse (cdr lst) (cons (car lst) new))))))

;; #5
;;5.1
(define empty-BST
	(lambda ()
		'()))
;;5.2
(define empty-BST?
	(lambda (obj)
		(and (null? obj) (list? obj))))

;;5.3
(define BST-insert
	(lambda (num bst)
		(cond
			[(empty-BST? bst) (list num (empty-BST) (empty-BST))]
			[(= (car bst) num) bst]
			[(< (car bst) num) (list (car bst) (BST-left bst) (BST-insert num (BST-right bst)))]
			[(> (car bst) num) (list (car bst) (BST-insert num (BST-left bst)) (BST-right bst))])))

;;5.4
(define BST-inorder
	(lambda (bst)
		(if (null? bst)
			(list)
			(append (BST-inorder (BST-left bst)) (list (car bst)) (BST-inorder (BST-right bst))))))

;;5.5
(define BST?
	(lambda (obj)
		(cond
			[(null? obj) #t]
			[(not (list? obj)) #f]
			[(not (= 3 (length obj))) #f]
			[(not (apply < (BST-inorder obj))) #f]
			[(not (and
				[number? (car obj)]
				[BST? (BST-left obj)]
				[BST? (BST-right obj)])) #f]
			[else #t])))

;;5.6
(define BST-element
	(lambda (bst)
		(car bst)))

(define BST-left
	(lambda (bst)
		(if (null? (list-ref bst 1))
			'()
			(list-ref bst 1))))

(define BST-right
	(lambda (bst)
		(if (null? (list-ref bst 2))
			'()
			(list-ref bst 2))))

;;5.7
(define BST-insert-nodes
	(lambda (bst nums)
		(if (null? nums)
			bst
			(BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))

;;5.8
(define BST-contains?
	(lambda (bst num)
		(cond
			[(empty-BST? bst) #f]
			[(eq? (BST-element bst) num) #t]
			[else (or (BST-contains? (BST-left bst) num)
					  (BST-contains? (BST-right bst) num))])))

;;#6
(define map-by-position
	(lambda (fn-list arg-list)
		(map 
			(lambda (function arg) (function arg)) fn-list arg-list)))

;;#7
;;helper
(define check-bintree
	(lambda (T)
		(cond
			((null? T) #t)
			((integer? T) #t)
			(else
				[if (eq? (length T) 3)
					(and (symbol? (car T)) (check-bintree (cadr T)) (check-bintree (caddr T)))
					#f]))))

(define bt-leaf-sum
	(lambda (T)
		(cond
			[(number? T) T]
			[(list? T) (+ (bt-leaf-sum (cadr T)) (bt-leaf-sum (caddr T)))]
			[else T])))

(define bt-inorder-list
	(lambda (T)
		(if (number? T)
			'()
			(append (bt-inorder-list (cadr T)) (list (car T)) (bt-inorder-list (caddr T))))))

(define bt-max
	(lambda (T)
		(if (number? T)
			T
			(max (bt-max (list-ref T 1)) (bt-max (list-ref T 2))))))

