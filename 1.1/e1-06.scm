#lang sicp
(define (average x y) (/ (+ x y) 2))

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x) x)))

(sqrt-iter 1.0 9) ; works as intended
(sqrt-iter2 1.0 9) ; infinite loop

; Alyssa's program leads to an infinite loop.
; Reason: applicative-order evaluation means that
; both then and else clauses (and the predicate)
; are evaluated by new-if before returning one of them.
; This leads to sqrt-iter2 calling itself
; infinitely many times.

; Moral of the story: the standard if clause evaluates
; the then / else clause only when implied by the
; predicate for a good reason :).