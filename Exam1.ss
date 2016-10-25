;; Exam 1
;; Joyce Xu

;;C1
(define find-min
	(lambda (ls)
		(cond
			[(null? (cdr ls)) (car ls)]
			[(< (car ls) (find-min (cdr ls))) (car ls)]
			[else (find-min (cdr ls))])))

(define find-max
	(lambda (ls)
		(cond
			[(null? (cdr ls)) (car ls)]
			[(> (car ls) (find-max (cdr ls))) (car ls)]
			[else (find-max (cdr ls))])))

(define range-of-numbers
	(lambda (ls)
		(- (find-max ls) (find-min ls))))

;;C2
(define get-domain
	[lambda (ls r) 
		(if [null? r]
			ls
			(if [equal? #f (member (caar r) ls)]
				(get-domain (append ls (list (caar r))) (cdr r))
				(get-domain ls (cdr r))))])

(define domain
	[lambda [r]
		(if [null? r]
			'()
			[get-domain '() r])])

(define symmetric-use-domain?
	[lambda (d r)
		(if [null? d]
			#t
			(if [equal? #f (member (list (car d) (reverse (list car d))) r)]
				#f
				(symmetric-use-domain? (cdr d) r)))])

(define symmetric?
	[lambda (r)
		(if [null? r]
			#t
			[symmetric-use-domain? (domain r) r])])

;;C3
(define symbols-with-n-count
	(lambda (ls n)
		[(null? ls) '()])


