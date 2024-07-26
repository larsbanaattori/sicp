#lang sicp

; Version using append
;(define (fringe items)
;  (cond ((null? items)
;         nil)
;        ((pair? (car items))
;         (append (fringe (car items)) (fringe (cdr items))))
;        (else
;         (append (list (car items)) (fringe (cdr items))))))

; More efficient (?) and elegant (?)
; which doesn't use append
(define (fringe items)
  (define (iter left right-result)
    (if (null? left)
        right-result
        ((if (pair? (car left))
             iter
             cons)
         (car left)
         (iter (cdr left) right-result))))
  (iter items nil))


(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))
