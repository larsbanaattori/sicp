#lang sicp
(define (f g) (g 2))

(define (square x) (* x x))

(f square)
(f (lambda (z) (* z (inc z))))

; (f f)
; (f 2)
; (2 2)
; error: 2 is not a procedure
