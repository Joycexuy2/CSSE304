;; Assignment 11a
;;Joyce Xu

;; #1a
(define-syntax my-let
	(syntax-rules ()
		[(_ ((x v) ...) e1 e2 ...)
		 ((lambda (x ...) e1 e2 ...)  v ...)]
		[(_ n ((x v) ...) exp) 
			(letrec ([n (lambda (x ...) exp)])
				(n v ...))]))

;; #1b
(define-syntax my-or
	(syntax-rules ()
		[(_) #f]
		[(_ e1) e1]
		;;[(_ exp e1 e2) e1]
		[(_ e1 e2 ...)
			(let ((x e1))
				(if x x (my-or e2 ...)))]))

;; #1c
(define-syntax +=
	(syntax-rules ()
		[(_) #f]
		[(_ e1 e2)
			(begin
				(set! e1 (+ e1 e2))
				e1)]))

;; #1d
(define-syntax return-first
	(syntax-rules ()
		[(_ e1) e1]
		[(_ e1 e2 ...)
			(let ((val e1))
				(begin e2 ... val))]))

;; #2
(load "chez-init.ss")
(define-datatype bintree bintree? ;;defined in EOPL page 50
	(leaf-node
 		(num integer?))
 	(interior-node
 		(key symbol?)
 		(left-tree bintree?)
 		(right-tree bintree?)))

(define bintree-to-list (lambda (bt) bt))

;; #3
(define leaf-sum
	(lambda (T)
		(cases bintree T
			[leaf-node (datum) datum]
			[interior-node (key left-tree right-tree) (+ (leaf-sum right-tree) (leaf-sum left-tree))])))

(define isleaf?
	(lambda (T)
		(cases bintree T
			[leaf-node (datum) #t]
			[interior-node (key left-tree right-tree) #f])))

(define max-interior-helper
	(lambda (T)
		(cases bintree T
			[leaf-node (datum) #f]
			[interior-node (key left-tree right-tree)
				(cond
					[(and (isleaf? left-tree) (isleaf? right-tree))
						(let ((s (leaf-sum T)))
							(cons 
								(cons 
									key
									s)
								s))]
					[(isleaf? right-tree);;if the right-tree is not leaf, and left-tree is leaf
						(let* ((left-max (max-interior-helper left-tree))
								[max-lsum (cdar left-max)] 
				  				[lsum (cdr left-max)]
				  				[total-sum (+ lsum (leaf-sum right-tree))])
						(if (< max-lsum total-sum)
							(cons
								(cons key total-sum)
								total-sum)
							(cons
								(car left-max)
								total-sum)))]
					[(isleaf? left-tree) ;;if the left-tree is not leaf, and right-tree is leaf
						(let* ((right-max (max-interior-helper right-tree))
								[max-rsum (cdar right-max)] 
				  				[rsum (cdr right-max)]
				  				[total-sum (+ rsum (leaf-sum left-tree))])
						(if (< max-rsum total-sum)
							(cons
								(cons key total-sum)
								total-sum)
							(cons
								(car right-max)
								total-sum)))]
						[else;;both left and right not leaf
							(let* 
								([right (max-interior-helper right-tree)] ;;get right tree max
				  				  	[left (max-interior-helper left-tree)];;get left tree max
				  				  	[max-rsum (cdr (car right))] ;;max of right sum
				  				  	[rsum (cdr right)] ;;get right tree sum
				  				  	[max-lsum (cdr (car left))] ;;max of left sum
				  				  	[lsum (cdr left)] ;;sum of left sum
				  				  	[total-sum (+ lsum rsum)])
				  				(if [< max-rsum max-lsum]
				  					(if [< max-lsum total-sum]
				  						(cons 
				  							(cons key total-sum) 
				  							total-sum)
				  						(cons 
				  							(car left) 
				  							total-sum))
				  					(if [< max-rsum total-sum]
				  						(cons 
				  							(cons key total-sum) 
				  							total-sum)
				  						(cons 
				  							(car right) 
				  							total-sum))))])])))

(define max-interior
	(lambda (T)
		(car (car (max-interior-helper T)))))

;;4a
(load "chez-init.ss"); This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

;;helper, check whether the list are all symbol
(define all-symbol?
	(lambda (ls)
		(if (null? ls)
			#t
			(if (symbol? (car ls))
				(all-symbol (cdr ls))
				#f))))

(define list-of-expression?
	[lambda (e)
		(if (list? e) (andmap expression? e) #f)])

(define lambda-var?
	[lambda (e)
		(cond 
			[(symbol? e) #t]
			[(pair? e)
			  	(let helper ([rest e])
			  		(cond 
			  			[(null? rest) #t]
			  			[(symbol? rest) #t]
			  			[else 
			  				(and (symbol? (car rest)) (helper (cdr rest)))]))]
			[else #f])])

(define-datatype expression expression?
	[var-exp (id symbol?)]
	[lit-exp (id lit?)]
	[lambda-exp (var (lambda (v) (or (symbol? v) (list-of-expression? v)))) 
		(body list-of-expression?)]
	[lambda-var-exp (var lambda-var?)]
	[set!-exp (id symbol?) (r-val-exp expression)]
	[if-else-exp (condition-exp expression?) 
		(true-exp expression?) (false-exp expression?)]
	[if-exp (condition-exp expression?) 
		(true-exp expression?)]
	[let-exp (ls list-of-expression?) 
		(body list-of-expression?)]
	[single-let-exp (var expression?) 
		(body expression?)]
	[named-let-exp (name expression?) 
		(ls list-of-expression?) (body list-of-expression?)]
	[let*-exp (ls list-of-expression?) 
		(body list-of-expression?)]
	[letrec-exp (ls list-of-expression?) 
		(body list-of-expression?)]
	[app-exp (rator expression?) 
		(rand list-of-expression?)])

(define lit?
	(lambda (e)
		(or (number? e)
			(string? e)
			(symbol? e)
			(boolean? e)
			(char? e)
			(vector? e)
			(null? e)
			(and (list? e)
				 (list? (cdr e))
				 (eq? 'quote (car e))))))


(define parse-exp         
  (lambda (datum)
    (cond
     [(lit? datum) (lit-exp datum)]
     ;;set! expression
     [(eqv? (1st datum) 'set!)
     	(cond
     		[(> (length datum) 3)
     			(eopl:error 'parse-exp "Too many parts: ~s" datum)]
 			[(= (length datum) 3)
 				(if (symbol? (2nd datum))
	     			(set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
					(eopl:error 'parse-exp "declaration in set-expression ~s must take a symbol" (2nd datum)))]
 			[else (eopl:error 'parse-exp "set expression ~s has incorrect arguments" datum)]
     			)]
     [(pair? datum)
      (cond
      	[(not (list? datum))
      		(eopl:error 'parse-exp "expression ~s is an improper list" datum)]
      	;;lambda expression
        [(eqv? (car datum) 'lambda)
        	(cond
        		[(< (length datum) 3)
        			(eopl:error 'parse-exp "lambda expression: ~s" datum)]
        		[(symbol? (2nd datum))
        			(lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
    			[(list? (2nd datum))
    				(if (andmap (lambda (v) (symbol? v)) (cadr datum))
						(lambda-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))
						(eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" datum))]
				[else (eopl:error 'parse-exp "Incorrect lambda syntax: ~s" datum)])]
		;;if expression
		[(eqv? 'if (1st datum))
			(cond
				[(= 2 (length datum))
					(eopl:error 'parse-exp "missing then and else parts: ~s" datum)]
				[(= 3 (length datum))
					(if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
				[(= 4 (length datum))
					(if-else-exp (parse-exp (2nd datum))
								 (parse-exp (3rd datum))
								 (parse-exp (4th datum)))]
				[else
					(eopl:error 'parse-exp "if-expression: ~s does not have the right format: condition, then, else" datum)])]
		;;let expression
		[(or (eqv? 'let (1st datum)) (eqv? 'letrec (1st datum)) (eqv? 'let* (1st datum)));;check whether the 1st datum meeet any let type
			(cond
				[(< (length datum) 3) (eopl:error 'parse-exp "let-expression has incorrect length")]
				[else
					(letrec ([parse-let
						(lambda (ls)
							(let helper ((rest ls))
								(cond
									[(null? rest) (list)]
									[(not (list? rest)) (eopl:error 'parse-exp "~s-list is not a proper list" (1st datum) rest)]
									[(not (list? (car rest))) (eopl:error 'parse-exp "declaration in ~s-list is not a proper list" (1st datum) (car rest))]
									[(not (= 2 (length (car rest)))) (eopl:error 'parse-exp "declaration in ~s-list must be in length of two ~s" (1st datum) (car rest))]
									[(not (symbol? (caar rest))) (eopl:error 'parse-exp "variable in ~s must be a symbol ~s" (1st datum) (caar rest))]
									[else
										(cons (single-let-exp (parse-exp (caar rest))
																(parse-exp (cadar rest)))
											(helper (cdr rest)))])))])
							(cond
								[(symbol? (2nd datum))
								(cond
									[(= 3 (length datum)) (eopl:error 'parse-exp "named-let-exp has incorrect length ~s" datum)]
									[(not (eqv? 'let (1st datum))) (eopl:error 'parse-exp "declaration in ~s must be a proper list ~s" (1st datum) (2nd datum))]
									[else (named-let-exp (var-exp (2nd datum))
														(parse-let (3rd datum))
														(map parse-exp (cdddr datum)))])]
								[(eqv? 'let (1st datum))
									(let-exp (parse-let (2nd datum)) (map parse-exp (cddr datum)))]
								[(eqv? 'let* (1st datum))
									(let*-exp (parse-let (2nd datum)) (map parse-exp (cddr datum)))]
								[else
									(letrec-exp (parse-let (2nd datum)) (map parse-exp (cddr datum)))]))])]
       	[else (app-exp (parse-exp (1st datum))
		      		(map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

;;#4b
(define unparse-exp
	(lambda (e)
		(cases expression e
			[var-exp (id) id]
			[lit-exp (id) id]
			[lambda-var-exp (var) var]
			[lambda-exp (var body) 
				(cons 'lambda (cons (if (symbol? var) var (map unparse-exp var)) 
									(map unparse-exp body)))]
			[set!-exp (id r-val-exp) 
						(list 'set! (unparse-exp id) (unparse-exp r-val-exp))]
			[if-exp (condition-exp true-exp) 
						(list 'if (unparse-exp condition-exp) (unparse-exp true-exp))]
			[if-else-exp (condition-exp true-exp false-exp) 
						(list 'if (unparse-exp condition-exp) (unparse-exp true-exp) (unparse-exp false-exp))]
			[let-exp (ls body) 
					(cons* 'let (map unparse-exp ls) (map unparse-exp body))]
			[single-let-exp (var body) 
						(list (unparse-exp var) (unparse-exp body))]
			[named-let-exp (name ls body) (cons 'let 
					(cons (unparse-exp name) (cons (map unparse-exp ls) (map unparse-exp body))))]
			[let*-exp (ls body) (cons* 'let* 
					(cons (map unparse-exp ls) (map unparse-exp body)))]
			[letrec-exp (ls body) (cons* 'letrec 
					(cons (map unparse-exp ls) (map unparse-exp body)))]
			[app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))])))




