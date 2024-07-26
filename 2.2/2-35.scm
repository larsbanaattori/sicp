#lang sicp

(define (accumulate op nil seq)
  (if (null? seq)
      nil
      (op (car seq) (accumulate op nil (cdr seq)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

(define x (list 1 (list 2 (list 3 4)) 5))
(define y (list 1 2 3))
(define z (list 1 (list 2) 3))
(count-leaves x)
(count-leaves y)
(count-leaves z)
(count-leaves nil)
