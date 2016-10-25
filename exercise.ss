(define un-lexical-address
	(lambda (exp)
		(letrec (
			[unbind
				(lambda (bound-vars depth n)
					(if (zero? depth)
						(car (list-ref bound-vars n))
						(list-ref (list-ref bound-vars depth) n)))]
			[convert-to-LcExp
				(lambda (exp bound-vars)
					(if (eqv? (cadr exp) 'free)
								(caddr exp)
								(unbind bound-vars (cadr exp) (caddr exp))))]
			[helper
				(lambda (exp bound-vars)
					(cond
						[(symbol? exp)
							exp]
						[(and (eqv? ': (car exp)) (eqv? 'free (cadr exp)))
							(caddr exp)]
						[(and (eqv? ': (car exp)) (number? (cadr exp)))
							(convert-to-LcExp exp bound-vars)]
						[(eqv? 'lambda (car exp))
							(list 'lambda (cadr exp) (helper (caddr exp) (cons (cadr exp) bound-vars)))]
						[(eqv? 'if (car exp))
							(list 'if (if (eqv? ': (car (cadr exp))) (convert-to-LcExp (cadr exp) bound-vars) (map (lambda (x) (helper x bound-vars)) (cadr exp))) (helper (caddr exp) bound-vars) (helper (cadddr exp) bound-vars))]
						[else
							(map (lambda (x) (helper x bound-vars)) exp)]))])
		(helper exp '()))))