#lang sicp

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (/ (- (cube guess) x) x)) 1e-10))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (test x)
  (cube (cube-iter 1.0 x)))

(test 8)
(test 8230487242)
(test -8)
(test 123456789)
