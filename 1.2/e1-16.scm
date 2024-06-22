#lang sicp
(define (even? x)
  (= (remainder x 2) 0))

(define (square x) (* x x))

(define (expt b n)
  (define (iter base exp product)
    (display (list base exp product))
    (newline)
    (cond ((= exp 0) product)
          ((even? exp) (iter (square base) (/ exp 2) product))
          (else (iter base (- exp 1) (* product base)))))
  (iter b n 1))

(expt 2 13)