;; Parsed expression datatypes

(define-datatype expression expression?
    [var-exp ; variable references
        (id symbol?)]
    [lit-exp        ; "Normal" data.  Did I leave out any types?
        (datum
            (lambda (x)
                (ormap (lambda (pred) (pred x))
                (list number? vector? boolean? symbol? string? pair? null?))))]
    [app-exp        ; applications
        (rator expression?)
        (rands (list-of expression?))]
    [quote-exp
        (datum always?)]
    [if-exp
        (test-exp expression?)
        (then-exp expression?)
        (else-exp always?)]
    [if-else-exp
        (test-exp expression?)
        (true-exp expression?)
        (false-exp expression?)]
    [let-exp
        (vars (list-of expression?))
        (exps (list-of expression?))
        (bodies (list-of expression?))]
    [lambda-exp
        (vars (lambda (x) (or (expression? x) ((list-of expression?) x))))
        (bodies (lambda (x) (or (expression? x) ((list-of expression?) x))))]
    [lambda-exp-improper
        (vars (list-of expression?))
        (extra-vars symbol?)
        (bodies (list-of expression?))]
    [cond-exp
        (exps (list-of expression?))]
    [case-exp
        (eva (lambda (x) (or (expression? x)((list-of expression?) x))))
        (cases (list-of expression?))])

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
    [prim-proc
        (name symbol?)]
    [closure
        (vars (lambda (x)
                (or (symbol? x) ((list-of symbol?) x) (pair? x))))
        (bodies (list-of expression?))
        (env environment?)]
    [closure-improper
        (vars (list-of symbol?))
        (improper-var symbol?)
        (bodies (list-of expression?))
        (env environment?)])
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))
