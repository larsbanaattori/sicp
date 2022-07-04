#lang sicp

(define (cont-frac-iter n d k)
  (define (iter i accum)
    (if (= i 0)
        accum
        (iter (dec i)
              (/ (n i)
                 (+ (d i) accum)))))
  (iter k 0))

(define (tan-cf x k)
  (let ((x2 (- (* x x))))
    (define (n i)
      (if (< i 2)
          x
          x2))
    (define (d i)
      (- (* 2 i) 1.0))
    (cont-frac-iter n d k)))

(tan-cf 1 10)
(tan 1)
(newline)

(tan-cf 0 10)
(tan 0)
(newline)

(tan-cf 1.57 10)
(tan 1.57)
