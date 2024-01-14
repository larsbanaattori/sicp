#lang sicp
(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (f x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((< y z) (sum-of-squares x z))
        (else (sum-of-squares x y))))

(f 1 2 3) ; 13
(f 1 3 2) ; 13
(f 3 1 2) ; 13
(f 3 2 1) ; 13
(f -1 0 1) ; 1
(f 0 -1 1) ; 1
(f -1 -2 -3) ; 5
