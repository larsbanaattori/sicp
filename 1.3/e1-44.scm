#lang sicp

(define (smooth f)
  (define dx 0.0001)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

(define (smooth-n-fold f n)
  ((repeated smooth n) f))
