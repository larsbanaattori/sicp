#lang sicp

; Version that yields a recursive process
(define (cont-frac n d k)
  (define (rec i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (rec (inc i))))))
  (rec 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10e4)

; 1/(golden ratio) with 4 decimal place accuracy is 0.6180.
; k = 10 is needed to reach this level of accuracy

; Here's a version that yields an iterative process
; The trick is that we iterate "backwards"
(define (cont-frac-iter n d k)
  (define (iter i accum)
    (if (= i 0)
        accum
        (iter (dec i)
              (/ (n i)
                 (+ (d i) accum)))))
  (iter k 0))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                10e4)
