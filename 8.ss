;;Assignment 8
;;Joyce Xu

;;#1a
;;Check whether it is a slist, if no, run the procedure as a whole, if yes, run procedure one by one.
(define slist-map
	(lambda (proc slist)
		[if (null? slist)
			'()
			[cond
				((not (list? slist)) (proc slist))
				(else
					[cons (slist-map proc (car slist)) (slist-map proc (cdr slist))])]]))

;;#1b
;; check each part in the slist, if just number or symbol, return itself
;;otherwise, do reverse in the list first. Apply this to the whole slist.
(define slist-reverse
	(lambda (slist)
		(if (null? slist)
			'()
			(reverse 
				(map
					(lambda (n)
						(if (list? n)
							(slist-reverse n)
							n))
					slist)))))

;;#1c
(define slist-paren-count
	(lambda (slist)
		(if(null? slist)
			2
			(cond
				[(symbol? (car slist)) (if (null? (cdr slist)) 2 (slist-paren-count (cdr slist)))]
				[(null? (cdr slist)) (+ (slist-paren-count (car slist)) 2)]
				[else (+ (slist-paren-count (car slist)) (slist-paren-count (cdr slist)))]))))

 ;;#1d
(define slist-depth
	(lambda (slist)
		(let depth-helper ([a slist])
			[cond
				((null? a) 1)
				((symbol? (car a)) (depth-helper (cdr a)))
				(else (max (+ 1 (depth-helper (car a))) (depth-helper (cdr a))))])))

;; #1e
(define slist-symbols-at-depth
	(lambda (slist d)
		(if (null? slist)
			'()
			(if (= 1 d)
				(if (symbol? (car slist))
						(cons (car slist) (slist-symbols-at-depth (cdr slist) 1))
						(slist-symbols-at-depth (cdr slist) 1))
				(if (symbol? (car slist))
					(slist-symbols-at-depth (cdr slist) d)
					(append (slist-symbols-at-depth (car slist) (- d 1)) (slist-symbols-at-depth (cdr slist) d)))))))

;; #2
(define group-by-two
	(lambda (ls)
		(cond
			[(null? ls) ls]
			[(= 1 (length ls)) (list ls)]
			[else
				(cons
					[list (car ls) (cadr ls)]
					[group-by-two (cddr ls)])])))

;; #3
;;helper method
;;use recursion to get the n-length list by reducing n for each time
(define get-n-length-list
	(lambda (old n new)
		(if(= 0 n)
			new
			(get-n-length-list (cdr old) (- n 1) (append new (list (car old)))))))

;;same idea for get the rest of the list by reducing n for each time
(define get-the-rest
	[lambda (ls n)
		(if [= 0 n]
			ls
			(get-the-rest (cdr ls) (- n 1)))])

;;append each time's n-length list
(define group-by-n
	(lambda (ls n)
		(cond [(null? ls) ls]
			  [(< (length ls) n) (list ls)]
			  [else 
			    (let ((sublist (get-n-length-list ls n '())))
			    	(cons
			    		sublist
			    		(group-by-n (get-the-rest ls n) n)))])))

;; #4
(define subst-leftmost
	(lambda (new old slist equality-pred?)
		[if (null? slist)
			'()
			(let helper((current (car slist))(left (cdr slist)))
				[cond
					[(null? left) (if (equality-pred? current old)
										(cons new left)
										(cons current left))]
					[(or (null? current) (symbol? current)) (if (equality-pred? current old) 
																(cons new left) 
																(cons current(helper (car left) (cdr left))))]
					[else (cons (helper (car current) (cdr current)) left)]])]))

