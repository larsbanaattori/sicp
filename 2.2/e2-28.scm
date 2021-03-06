#lang sicp
(define (fringe tree)
  (define (iter tree result)
    (cond ((null? tree)
           result)
          ((pair? tree)
           (iter (car tree)
                 (iter (cdr tree) result)))
          (else (cons tree result))))
  (iter tree nil))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))
(fringe (list 1 (list 2 (list (list 3 4) 5))))
