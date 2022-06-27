#lang sicp
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 1 2)
(a-plus-abs-b 1 -2)

; The idea is to take care of the absolute value
; part of the function by selecting the operator
; to be applied on a and b based on the value of b