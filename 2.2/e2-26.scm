#lang sicp
(define x (list 1 2 3))
(define y (list 4 5 6))

(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

(append x y) ; (1 2 3 4 5 6)
(cons x y) ; ((1 2 3) 4 5 6)
(list x y) ; ((1 2 3) (4 5 6))
