#lang sicp
(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (/ (abs (- (cube guess) x))
        x)
     10e-9))

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (abs-error x)
  (abs (- x (cube (cube-root x)))))

(define (rel-error x)
  (/ (abs-error x) x))

(define (test x)
  (if (< (rel-error x) 10e-6)
      'OK
      'ERROR))

(display 'Tests:)
(newline)
(test 3)
(test 12e20)
(test 12e-20)
(newline)

(display "cube-root(8)")
(newline)
(cube-root 8)

(display "cube-root(1881365963625)")
(newline)
(cube-root 1881365963625)