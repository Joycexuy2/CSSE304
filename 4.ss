;; Assignment 04
;; Joyce Xu

;; Problme 1
(define multi-set? 
	(lambda （n)
		(cond
			((not (list? n)) #f)
			((null? n) #t)
			((not (not (member #f (map list? n)))) #f)
			((not (not (member #f (map positive? (map cadr n))))) #f)
			((not (member (caar n) (map car (cdr n)))) (multi-set (cdr n)))
			(else
				#f)))

;; Problem 2
(define ms-size
	(lambda (ms)
		)