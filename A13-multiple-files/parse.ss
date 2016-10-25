; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4rd cadddr)

;;(define parse-exp         
;  (lambda (datum)
;    (cond
;     [(symbol? datum) (var-exp datum)]
;     [(number? datum) (lit-exp datum)]
;     [(pair? datum)
;      (cond
;      
;       [else (app-exp (parse-exp (1st datum))
;		      (map parse-exp (cdr datum)))])]
;     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))
;;helper, check whether the list are all symbol
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
			(list? e))))

(define parse-exp
  (lambda (datum)
    (cond
     [(and (list? datum) (eq? 'quote (1st datum)))
     	(if (> (length datum) 2)
     		(eopl:error 'parse-exp "Invalid syntax for quote: ~s" datum)
     		(lit-exp (cadr datum)))]
 	 [((lambda (e)
 	 	(or (number? e)
			(string? e)
			(boolean? e)
			(char? e)
			(vector? e))) datum) (lit-exp datum)]
 	 [(symbol? datum) (var-exp datum)]
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










