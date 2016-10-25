;; Assignment 02
;; Joyce Xu

;;Question 1 
(define fact
	(lambda (n)
		(if (zero? n(
			1
			(* n (fact (- n 1))))))))

(define choose
	(lambda (n k)
		(/ (fact n) ((fact (- n k)) (fact k)))))