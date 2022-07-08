#lang sicp
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list-2 items)
  (map square items))

(define (square x) (* x x))

(square-list (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))
