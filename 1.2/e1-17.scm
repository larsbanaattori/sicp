#lang sicp

(define (even? x) (= (remainder x 2) 0))

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (dec b))))))

(fast-mul 100 23554)
