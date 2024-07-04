#lang sicp

(define (cont-frac n d k)
  (define (rec i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (rec (inc i))))))
  (rec 1))

(define (f i)
  (let ((i (inc i)))
    (if (= (remainder i 3) 0)
        (* 2 (/ i 3))
        1)))

(define (e-approx k)
  (+ 2.0 (cont-frac (lambda (x) 1) f k)))

(e-approx 1000) ; close enough
