#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

(define (tree-map proc tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map proc x)
             (proc x)))
       tree))

(define (square x) (* x x))

(define (square-tree tree) (tree-map square tree))

(square-tree
       (list 1
             (list 2 (list 3 4) 5)
             (list 6 7)))
