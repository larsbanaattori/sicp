#lang sicp
(define (double f)
  (lambda (x) (f (f x))))

((double inc) 0)

(((double (double double)) inc) 5) ; 21

; double: f -> f o f = f^2
; (double double): f -> (f o f) o (f o f) = f^4
; (double (double double)): f -> (f^4 o f^4) o (f^4 o f^4) = f^16
