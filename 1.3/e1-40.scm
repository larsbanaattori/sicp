#lang sicp
(define tolerance 0.00001)
(define dx 0.00001)

(define (fixed-point f initial-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try initial-guess))

(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x)
            ((deriv f) x)))))

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f (- x dx)))
       (* 2 dx))))

(define (newtons-method f guess)
  (fixed-point (newton-transform f)
               guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (cube x) (* x x x))

(define (square x) (* x x))

(newtons-method (cubic 1 2 3) 1.0) ; ~1.276
