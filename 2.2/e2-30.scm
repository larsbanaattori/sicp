#lang sicp
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

(define (square x) (* x x))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)

       (list 6 7)))

(define (map proc list)
  (if (null? list)
      nil
      (cons (proc (car list)) (map proc (cdr list)))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)

       (list 6 7)))
