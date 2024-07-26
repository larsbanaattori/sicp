#lang sicp

(define (filter items f)
  (cond ((null? items)
         nil)
        ((f (car items))
         (cons (car items) (filter (cdr items) f)))
        (else
         (filter (cdr items) f))))

(define (same-parity i . items)
  (define (f item)
    (= (remainder i 2) (remainder item 2)))
  (filter (cons i items) f))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
