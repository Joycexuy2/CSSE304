(define group-by-two
	(lambda (ls)
		(if (null? ls)
			'()
			(if (null? (cdr ls))
				(list (list (car ls)))
				(cons (list (car ls) (cadr ls)) (group-by-two (cddr ls))))))) ;done
(define group-by-n
	(lambda (ls n)
		(if (null? ls)
			'()
			(if (< (length ls) n)
				(list ls)
				(cons (group-maker ls n 0) (group-by-n (group-cdr ls n 0) n)))))) ;done
(define group-maker
	(lambda (ls n c)
		(if (= n c)
			'()
			(cons (car ls) (group-maker (cdr ls) n (+ 1 c)))))) ;done
(define group-cdr
	(lambda (ls n c)
		(if (= n c)
			ls
			(group-cdr (cdr ls) n (+ 1 c))))) ;done
(define sorted?
	(lambda (ls)
		(if (or (null? ls) (null? (cdr ls)))
			#t
			(andmap < ls (sort-helper (cdr ls))))))
(define sort-helper
	(lambda (ls)
		(if (null? (cdr ls))
			(list (car ls) (+ 1 (car ls)))
			(append (list (car ls)) (sort-helper (cdr ls))))))

(define sorted2?
	(lambda (ls)
		(apply < ls))) ;done
(define all-=?
	(lambda (ls)
		(if (or (null? ls) (null? (cdr ls)))
			#t
			(if (ormap symbol? ls)
				#f
				(apply = (car ls) (cdr ls))))))
(define max-path-from-root-to-leaf
	(lambda (t)
		(if (null? t)
			0
			(max (+ (car t) (max-path-from-root-to-leaf (cadr t))) (+ (car t) (max-path-from-root-to-leaf (caddr t)))))));done