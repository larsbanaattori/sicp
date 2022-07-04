#lang sicp
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

(define dx 0.01)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (smooth-n-fold f n)
  ((repeated smooth n) f))

(define (cube x) (* x x x))

((smooth cube) 0.6)

((smooth-n-fold cube 3) 0.6)
