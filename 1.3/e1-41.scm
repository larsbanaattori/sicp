#lang sicp
(define (double f)
  (lambda (x)
    (f (f x))))

(((double (double double)) inc) 5)
(((double (double (double double))) inc) 0)

; double: f(x) -> (f o f)(x)
; (double double) -> ((f o f) o (f o f))(x) = (f o f o f o f)(x)
; (double (double double)) -> ((f o f o f o f) o (f o f o f o f) o (f o f o f o f) o (f o f o f o f))(x)
;  -> inc is applied 16 times
; (((double (double double)) inc) 5) = 21
