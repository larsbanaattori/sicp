#lang sicp
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; one =
; (add-1 zero)
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) ; ((lambda (f) (lambda (x) x)) f) = (lambda (x) x)
; (lambda (f) (lambda (x) (f ((lambda (x) x) x)))) ; ((lambda (x) x) x) = x
; (lambda (f) (lambda (x) (f x))) ; (lambda (x) (f x)) = f
; (lambda (f) f)
(define one (lambda (f) f))
((one inc) 0) ; test, should be 1

; two =
; (add-1 one)
; (lambda (f) (lambda (x) (f (((lambda (f) f) f) x)))) ; ((lambda (f) f) f) = f
(define two (lambda (f) (lambda (x) (f (f x)))))
((two inc) 0) ; test, should be 2

; in a nutshell, "one" is f -> f and "two" is f -> (f o f)
; "add-1" is (f -> nf) -> (f -> (n+1)f)

(define (+ n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

(((+ one two) inc) 0) ; test, should be 3
