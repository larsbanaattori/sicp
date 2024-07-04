#lang sicp

(define (cont-frac n d k)
  (define (rec i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (rec (inc i))))))
  (rec 1))

(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (- (* x x))))
  (define (d k)
    (- (* k 2) 1))
  (cont-frac n d k))

(define (test x)
  (let ((approx (tan-cf x 100))
        (truth (tan x)))
    (let ((error (abs (- approx truth))))
      (display (list approx truth error))
      (newline))))

(test 0)
(test 0.25873)
(test -1.3994)
(test 2.3234)
