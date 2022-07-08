#lang sicp
(define (reverse items)
  (define (iter head tail)
    (if (null? head)
        tail
        (iter (cdr head) (cons (car head) tail))))
  (iter items nil))

(reverse (list 1 4 9 16 25))
