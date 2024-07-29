#lang sicp

(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 1 '())
(adjoin-set 1 '(2 3))
(adjoin-set 1 '(1 2 3))
(adjoin-set 2 '(1 3 5))
(adjoin-set 6 '(1 3 5))

; In the average case, adjoin-set goes through half of the
; items in set until x <= (item of set), which halts
; the iteration
