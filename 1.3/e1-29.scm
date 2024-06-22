#lang sicp

(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a) (sum f (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (cube x) (* x x x))

(define (integral-simpson f a b dx)
  (define (term x)
    (+ (f (- x dx)) (* 4 (f x)) (f (+ x dx))))
  (define (next x)
    (+ x (* 2 dx)))
  (* (sum term (+ a dx) next b) (/ dx 3.0)))

(integral cube 0 1 0.01)
(integral-simpson cube 0 1 0.01)
(newline)

(abs (- (/ (integral cube 0 1 0.01) 0.25) 1))
(abs (- (/ (integral-simpson cube 0 1 0.01) 0.25) 1))
(newline)

(integral cube 0 1 0.001)
(integral-simpson cube 0 1 0.001)
(newline)

(abs (- (/ (integral cube 0 1 0.001) 0.25) 1))
(abs (- (/ (integral-simpson cube 0 1 0.001) 0.25) 1))