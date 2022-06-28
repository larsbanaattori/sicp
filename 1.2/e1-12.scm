#lang sicp

(define (pascal n k)
  (if (or (= n k) (= k 0))
      1
      (+ (pascal (dec n) k) (pascal (dec n) (dec k)))))