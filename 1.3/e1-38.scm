#lang sicp

(define (cont-frac-iter n d k)
  (define (iter i accum)
    (if (= i 0)
        accum
        (iter (dec i)
              (/ (n i)
                 (+ (d i) accum)))))
  (iter k 0))

(define (d i)
  (if (= (remainder (inc i) 3) 0)
      (* 2.0 (/ (inc i) 3))
      1.0))

(define (n i) 1.0)

(define (abs-error k)
  (abs (- (exp 1)
          (+ 2 (cont-frac-iter n d k)))))

(abs-error 5)
(abs-error 10)
(abs-error 50)
