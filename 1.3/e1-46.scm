#lang sicp

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
        guess
        (try (improve guess))))
  try)

(define (sqrt x)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- (square guess) x)) 0.001))
    (lambda (guess)
      (average guess (/ x guess))))
   1.0))

(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(sqrt 16)

(define (fixed-point f guess)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- guess (f guess))) 0.00001))
    f)
   guess))

(define (sqrt-fp x)
  (fixed-point
   (lambda (guess)
     (average guess (/ x guess)))
   1.0))

(sqrt-fp 16)
