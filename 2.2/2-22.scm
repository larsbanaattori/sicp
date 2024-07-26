#lang sicp

(define square (lambda (x) (* x x)))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define x (list 1 2 3 4 5))

(square-list x)

; This version does square the list, but it also reverses it.
; Reversing happens because the result list is accumulated
; from end to start while iter goes through the original list
; from start to end

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list2 x)

; This doesn't generate a list since (cons {list} {atom})
; doesn't yield a list, but rather a pair with list as first element
; and an atom as the second one
