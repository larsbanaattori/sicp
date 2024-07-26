#lang sicp

(define (map f items)
  (if (null? items)
      nil
      (cons
       (f (car items))
       (map f (cdr items)))))

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))

(define x (list 1 2 3 4 5))

(square-list x)
(square-list2 x)
