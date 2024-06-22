#lang sicp

(define (even? x) (= (remainder x 2) 0))

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (fast-mul x y)
  (define (iter a b sum)
    (display (list a b sum))
    (newline)
    (cond ((= b 0) sum)
          ((even? b) (iter (double a) (halve b) sum))
          (else (iter a (dec b) (+ sum a)))))
  (iter x y 0))

(fast-mul 100 65536)
(newline)
(fast-mul 10 99)