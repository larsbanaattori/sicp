#lang sicp

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else
         (union-set (cdr s1) (cons (car s1) s2)))))

(define (element-of-set? item set)
  (cond ((null? set) #f)
        ((equal? item (car set)) #t)
        (else (element-of-set? item (cdr set)))))

(union-set '(1 2 3) '(3 4 5))
(union-set '() '(3 4 5))
(union-set '(1 2 3) '())
(union-set '() '())
