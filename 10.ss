;;Assignment 10a
;;Joyxu Xu

;#1a
(define avoid-duplicate
	(lambda (ls)
		(let helper ((ls ls) (lst (list)))
			(if (null? ls)
				lst
				[if (member (car ls) lst)
					(helper (cdr ls) lst)
					(helper (cdr ls) (append lst (list (car ls))))]))))

(define free-vars
	(lambda (ls)
		(avoid-duplicate 
			(let helper ((ls ls) (vars (list)))
				(cond 
					[(null? ls) (list)]
					[(symbol? ls)
					     (if (member ls vars)
					     	 (list)
					     	 (list ls))]
					[(eq? (car ls) 'lambda) (helper 
												(caddr ls) 
												(append vars (cadr ls)))]
					[else (append (helper (car ls) vars) (helper (cadr ls) vars))])))))

;#1b
(define bound-vars
	(lambda (ls)
		(avoid-duplicate 
			(let helper ((ls ls) (vars (list)))
				(cond 
					[(null? ls) (list)]
					[(symbol? ls)
					     (if (member ls vars)
					     	 (list ls)
					     	 (list))]
					[(eq? (car ls) 'lambda) (helper 
												(caddr ls) 
												(append vars (cadr ls)))]
					[else (append (helper (car ls) vars) (helper (cadr ls) vars))])))))

;#2a
(define occurs-free?
  	(lambda (var exp)
    	(cond
    		[(null? exp) #f]
      		((symbol? exp) (eqv? var exp))
      		((eqv? (car exp) 'lambda) 
       		 (and (not (eqv? (caadr exp) var))
            	  (occurs-free? var (caddr exp))))
      		[(eqv? (car exp) 'set!) 
      			(and #f (or (occurs-free? var (cadr exp))
      			(occurs-free? var (caddr exp))))]
      		[(eqv? (car exp) 'let*) 
      			(or (let helper ([ls (cadr exp)])
      				(cond [(null? ls) #f]
      					[(occurs-free? var (cadar ls)) #t]
      					[(eqv? (caar ls) var) #f]
      					[else (helper (cdr ls))]))
     	 		(and (not (member var (map car (cadr exp))))
      			 	(occurs-free? var (caddr exp))))]
      		(else (or (occurs-free? var  (car exp))
                	  (occurs-free? var (cadr exp)))))))

;#2b
(define occurs-bound?
  	(lambda (var exp)
    	(cond
    		[(null? exp) #f]
      		[(symbol? exp) #f]
      		[(eqv? (car exp) 'lambda)
       			(or (occurs-bound? var (caddr exp))
           			(and (member (caadr exp) var)
                		 (occurs-free? var (caddr exp))))]
   			[(eqv? (car exp) 'if)
   				(or (occurs-bound? var (cadr exp))
   					(occurs-bound? var (caddr exp))
   					(occurs-bound? var (cadddr exp)))]
  			[else (or (occurs-bound? var  (car exp))
                	  (occurs-bound? var (cadr exp)))])))

; #3
(define lexical-address
	(lambda (e)
		(letrec (
			[get-index (lambda (var ls)
				(letrec ((checkList (lambda (var ls num)
					(if (null? ls)
						#f
						(if (eq? var (car ls))
							num
							(checkList var (cdr ls) (+ 1 num)))))))
				(checkList var ls 0)))]
			[get-bound (lambda (item bound-vars depth)
				(if (null? bound-vars)
						(list ': 'free item)
						(let ((index (get-index item (car bound-vars))))
							(if index
								(list ': depth index)
								(get-bound item (cdr bound-vars) (+ depth 1))))))]
			[helper (lambda (e bound-vars)
				(cond
					[(symbol? e)
						(get-bound e bound-vars 0)] ;;initialize the depth to 0
					[(eq? 'lambda (car e))
						(list 'lambda (cadr e) (helper (caddr e) (cons (cadr e) bound-vars)))]
					[(eq? 'if (car e))
							(list 'if (if (pair? (cadr e)) 
								(map (lambda (x) (helper x bound-vars)) (cadr e)) 
								(helper (cadr e) bound-vars)) (helper (caddr e) bound-vars) (helper (cadddr e) bound-vars))]
					[(eq? 'let (car e))
						(let ([var-list (map car (cadr e))]
							  [val-list (map cadr (cadr e))])
							(let ([processed-val-list (map (lambda (e) (helper e bound-vars)) val-list)]
								  [processed-body (map (lambda (x) (helper x (cons var-list bound-vars))) (cddr e))])
								(cons* 'let (map (lambda (var val) (list var val)) var-list processed-val-list) processed-body))
								)]
					[else
						(map (lambda (x) (helper x bound-vars)) e)]))])
		(helper e '()))))

;;#4
(define un-lexical-address
	(lambda (e)
		(letrec (
			[bound-back
				(lambda (bound-vars depth n)
					(if (eq? 0 depth)
						(car (list-ref bound-vars n))
						(list-ref (list-ref bound-vars depth) n)))]
			[convert
				(lambda (e bound-vars)
					(if (eq? (cadr e) 'free)
						(caddr e)
						(bound-back bound-vars (cadr e) (caddr e))))]
			[helper
				(lambda (e bound-vars)
					(cond
						[(symbol? e) e]
						[(and (eq? ': (car e)) (number? (cadr e))) (convert e bound-vars)]
						[(and (eq? ': (car e)) (eq? 'free (cadr e))) (caddr e)]
						[(eq? 'lambda (car e)) 
							(list 'lambda (cadr e) (helper (caddr e) (cons (cadr e) bound-vars)))]
						[(eq? 'if (car e))
							(list 'if (if (eq? ': (car (cadr e))) (convert (cadr e) bound-vars) 
								(map (lambda (x)
									(helper x bound-vars)) (cadr e)))
							(helper (caddr e) bound-vars) (helper (cadddr e) bound-vars))]
						[else
							(map (lambda (x)
								(helper x bound-vars)) e)]
						))])
		(helper e '()))))

