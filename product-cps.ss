(define product-cps
	(lambda (x y k)
		(if (null? y) 
			(apply-continuation k '())
			(let loop ([x x] [accum '()])
				(if (null? x) 
					(apply-continuation k accum)
					(loop (cdr x)
						(map-cps (lambda (s) (list (car x) s)) y (lambda (v) (append-cps v accum k)))))))))

(define product-cps
	(lambda (x y k)
		(if (null? y)
			(apply-continuation k '())
			(product-cps
				x
				(cdr y)
				(lambda (cdr-y)
					(combine-cps (car y) x
						(lambda (one-y)
							(append-cps one-y cdr-y k))))))))

(define combine-cps
	(lambda (item ls k)
		(if (null? ls)
			(apply-continuation k ls)
			(combine-cps
				item
				(cdr ls)
				(lambda (combined-cdr)
					(append-cps (list (list item (car ls))) combined-cdr k))))))

(define apply-continuation
	(lambda (k . v)
		(apply k v)))

(define map-cps
	(lambda (proc-cps ls k)
		(cond
			[(null? ls) (apply-continuation k '())]
			[else
				(map-cps proc-cps (cdr ls) 
								(lambda (cdr-ls)
									(proc-cps (car ls)
										(lambda (result)
											(apply-continuation k (cons result cdr-ls))))))])))

(define loop-cps
	(lambda (setX setY proc k)
		(if (null? setX)
			(apply-continuation k setY)

(d)
