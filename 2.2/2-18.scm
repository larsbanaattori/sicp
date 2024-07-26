#lang sicp

(define (reverse items)
  (define (iter tail result)
    (if (null? tail)
        result
        (iter (cdr tail) (cons (car tail) result))))
  (iter items nil))

(define squares (list 1 4 9 16 25))
(reverse squares)

(reverse nil)

(reverse (list 1 2))

(reverse (list 1))
