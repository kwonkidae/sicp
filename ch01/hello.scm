;(display "Hello World, this is my first Scheme program!!!")
(* 4 4)

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
  (+ (- 10 7)
    6))

; (define variable value)
(display "\n")
(define size 2)
(display size)
(display "\n")

(define (square x ) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(display (sum-of-squares 4 3))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;    guess
;    (sqrt-iter (improve guess x) x)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))


(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 4)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
