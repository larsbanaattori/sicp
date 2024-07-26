#lang sicp

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (x)
         (if (not (pair? x))
             (square x)
             (square-tree2 x)))
       tree))

(square-tree
       (list 1
             (list 2 (list 3 4) 5)
             (list 6 7)))

(square-tree2
       (list 1
             (list 2 (list 3 4) 5)
             (list 6 7)))
