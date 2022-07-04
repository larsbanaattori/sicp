#lang sicp

; Recursive process version
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(product (lambda (x) (/ (* (- x 1.0) (+ x 1.0))
                        (* x x)))
         3.0
         (lambda (x) (+ x 2))
         1000000)
; 0.78539816339 is the "right" answer
; Close enough

; Iterative process version
(define (product-iter term a next b)
  (define (iter a accum)
    (if (> a b)
        accum
        (iter (next a)
              (* accum
                 (term a)))))
  (iter a 1))

(product-iter (lambda (x) (/ (* (- x 1.0) (+ x 1.0))
                             (* x x)))
              3.0
              (lambda (x) (+ x 2))
              1000000)
