#lang sicp
(define (f x y z)
  (define (sq x) (* x x))
  (define (sos x y) (+ (sq x) (sq y)))
  (cond ((and (> x z) (> y z)) (sos x y))
        ((and (> x y) (> z y)) (sos x z))
        (else (sos y z))))

(define (test x y z result)
  (if (= (f x y z) result)
      'OK
      'ERROR))

(test 1 2 3 13)
(test 1 3 2 13)
(test 3 1 2 13)
(test 3 2 1 13)
(test 0 0 0 0)
(test 8 5 1 89)