#lang sicp
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

; Applicative-order evaluation
(test 0 (p))
; -> tries to evaluate (p)
; -> (p) -> (p) -> ... infinite loop

; Normal order evaluation
(test 0 (p))
(if (= 0 0) 0 (p))
; Predicate is evaluated first. (= 0 0) = #t. ->
; Consequent is evaluated ->
0

; Normal order evaluation leads to (p) never being evaluated.
; Hence, we avoid the infinite loop unlike with
; applicative-order evaluation
