
(define free-vars 
	(lambda (exp)
  	(cond 
		[(null? exp) '()]
      	[(number? exp) 'num '()]
		[(symbol? exp) (list exp)]	
     	[(equal? (car exp) 'let)
			(cond 
				[(null? (cadr exp)) (free-vars (caddr exp))]
          		[else (union-set (free-vars (let-exps exp))
                  				 (remove-elements-from-set (let-vars exp)
                       			(free-vars (caddr exp))))])]
      	[(equal? (car exp) 'let*)
        	(cond 
				[(null? (cadr exp)) (free-vars (caddr exp))]
          		[else (union-set (free-vars (cadr (caadr exp)))
                      		 	(remove-element-from-set (caaadr exp)
                       			(free-vars (list 'let* (cdr (cadr exp)) (caddr exp)))))])]
																										
      	[(equal? (car exp) 'lambda) (remove-elements-from-set (cadr exp) (free-vars (caddr exp)))]
		[(equal? (car exp) 'if) (union-set (free-vars (cadr exp))
											(union-set (free-vars (caddr exp)) 
											(free-vars (cadddr exp))))]
		[else (union-set (free-vars (car exp)) (free-vars (cdr exp)))])))