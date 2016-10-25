(define apply-continuation
	(lambda (k . v)
		(apply k v)))

(define append-cps
	[lambda (ls1 ls2 k)
		(cond [(null? ls2) (apply-continuation k ls1)]
			  [else 
			  	(append-cps 
			  		(reverse (cons (car ls2) (reverse ls1)))
			  		(cdr ls2)
			  		k)])])

(define product-cps
	(lambda (x y k)
		(if (null? y) 
			(apply-continuation k '())
			(let loop ([x x] [accum '()])
				(if (null? x) 
					(apply-continuation k accum)
					(loop (cdr x)
						(map-cps (lambda (s) (list (car x) s)) y (lambda (v) (append-cps v accum k)))))))))