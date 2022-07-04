#lang sicp
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a)))))
  (iter a 0))

; Test with sum of cubes from 1 to 10
(sum (lambda (x) (* x x x))
     1
     inc
     10)