;;Assignment 9
;;Joyce Xu

(define snlist-recur
	(lambda (base-value list-proc other-proc)
		(letrec
			((helper (lambda (sn-list)
				(cond 
					[(null? sn-list) base-value]
					[(list? (car sn-list)) (list-proc (helper (car sn-list)) (helper (cdr sn-list)))]
					[else (other-proc (car sn-list) (helper (cdr sn-list)))]))))
			helper)))
;;#1a
(define sn-list-sum
	(snlist-recur 0 + +))

;;#1b
(define sn-list-map
	(lambda (proc ls)
		((snlist-recur '() cons 
			(lambda (x y)
				(cons (proc x) y))) ls)))

;;#1c
;;A "()" is the base case, so put two for the based case
(define sn-list-paren-count
	(snlist-recur 2 
		(lambda (x y)
			(+ x y))
		(lambda (x y)
			y)))


;;#1d
(define sn-list-reverse 
	(lambda (ls)
		(reverse ((snlist-recur '() 
				(lambda (start end)
					(cond
						[(list? start) (cons (reverse start) end)]
						[else (cons start end)]))
				cons) ls))))

;;#1e
(define sn-list-occur
	(lambda (s ls)
		((snlist-recur 0 + 
			(lambda (x y) 
				(if (eq? s x)
					(+ 1 y)
					y))) ls)))

;;#1f
(define sn-list-depth
	(lambda (ls)
		(+ 1 ((snlist-recur 0 
			(lambda (start end) (max (+ 1 start) end))
			(lambda (start end) (max 0 end)))ls))))

;;#2
(define bt-recur
	(lambda (proc1 proc2)
		(letrec ((helper (lambda (bt)
							[if (number? bt)
								(proc1 bt)
								(proc2 
									(car bt) 
									(helper (cadr bt)) 
									(helper (caddr bt)))])))
				helper)))

;;#2a
(define bt-sum
	(bt-recur 
		+ 
		(lambda (x y z) (+ y z))))

;;#2b
(define bt-inorder
	(bt-recur 
		(lambda (x) (list)) 
		(lambda (x y z) (append y (list x) z))))

;;#3
(define helper (lambda (x)
	(if (eq? 0 (string-length x))
		(lambda (v) v)
		(cond 
			[(equal? '#\a (string-ref x 0))
				(lambda (v) 
					((helper (substring x 1 (string-length x)))(car v)))]
			[(equal? '#\d (string-ref x 0))
				(lambda (v) 
					((helper (substring x 1 (string-length x)))(cdr v)))]
			[else (#f)]))))

(define make-c...r (lambda (x)
	(helper (list->string
		(reverse (string->list x))))))



