#lang sicp

; 1. Version that generates a recursive process
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; Checks
(sum (lambda (x) (* x x x)) 1 inc 10) ; 3025
(product (lambda (x) x) 1 inc 10) ; 3628800

; 2. Version that generates an iterative process
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a accum)
    (if (> a b)
        accum
        (iter (next a) (combiner accum (term a)))))
  (iter a null-value))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

; Checks
(sum-iter (lambda (x) (* x x x)) 1 inc 10) ; 3025
(product-iter (lambda (x) x) 1 inc 10) ; 3628800