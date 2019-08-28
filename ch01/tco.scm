(define (factorial_linear_recursive n)
	(if (= n 1)
		1
		(* n (factorial_linear_recursive (- n 1)))))

(define (factorial n)
	(fact-iter 1 1 n))

(define (fact-iter product counter max-count)
	(if (> counter max-count)
		product
		(fact-iter (* counter product)
							 (+ counter 1)
							 max-count)))


(display 
	((lambda (x) 
		(define (factorial n)
			(define (iter product counter)
				(if (> counter n)
					product
					(iter (* counter product)
								(+ counter 1))))
			(iter 1 1))
		(factorial x))
	10))

(define (A x y)
	(cond ((= y 0) 0)
				((= x 0) (* 2 y))
				((= y 1) 2)
				(else (A (- x 1) (A x (- y 1))))))