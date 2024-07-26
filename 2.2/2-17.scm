#lang sicp

(define (last-pair items)
  (let ((tail (cdr items))
        (head (car items)))
    (if (null? tail)
        head
        (last-pair tail))))

(define squares (list 1 4 9 16 25))
(last-pair squares)

(last-pair (list 23 72 149 34))

(define singleton (list 1))
(last-pair singleton)
