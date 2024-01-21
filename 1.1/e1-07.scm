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

(define (error x)
  (abs (/ (- x (square (sqrt-iter 1.0 x))) x)))

(error 9)
(error 9e-2)
(error 9e-4) ; huge error
(error 9e-8) ; enormous error

;(sqrt-iter 1.0 9e60) ; doesn't halt

; Problem with very small numbers:
; big error since the error tolerance is absolute
; and, when trying to sqrt small enough numbers,
; the iteration stops way short.
;
; Problem with big numbers:
; infinite loop since the limited accuracy of
; floating point numbers doesn't allow us to
; get close enough to the right answer for the
; iteration to halt.
;
; Solution: let's use relative error instead
; of absolute in good-enough. This ensures
; that the error is OK for small inputs
; and iteration should converge with large inputs

(define (good-enough2? guess x)
  (< (/ (abs (- (square guess) x)) x) 1e-9))

(define (sqrt-iter2 guess x)
  (if (good-enough2? guess x)
      guess
      (sqrt-iter2 (improve guess x) x)))

(define (error2 x)
  (abs (/ (- x (square (sqrt-iter2 1.0 x))) x)))

(newline)
(error2 9e-8)
(error2 9e-16)
(error2 9e40)
(sqrt-iter2 1.0 9e-16)
(sqrt-iter2 1.0 9e40)
