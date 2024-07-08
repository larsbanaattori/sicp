#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; one
;(add-1 zero)
;(add-1 (lambda (f) (lambda (x) x)))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) ; ((lambda (f) (lambda (x) x)) f) = (lambda (x) x)
;(lambda (f) (lambda (x) (f ((lambda (x) x) x)))) ; ((lambda (x) x) x) = x
;(lambda (f) (lambda (x) (f x)))
;(lambda (f) f)
(define one (lambda (f) f))
((one inc) 0) ; should be one

; two
;(add-1 one)
;(add-1 (lambda (f) f))
;(lambda (f) (lambda (x) (f (((lambda (f) f) f) x)))) ; ((lambda (f) f) f) = f
;(lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
((two inc) 0) ; should be two

; general addition
(define (add n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

(((add one two) inc) 0)
