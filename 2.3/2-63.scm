#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define tl (make-tree 3 (make-tree 1 '() '()) '()))
(define tr (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '())))
(define t (make-tree 5 tl tr))

(tree->list-1 t)
(tree->list-2 t)

; (a)
; Both versions produce the same result since they
; list the tree "left-to-right" i.e. from smallest to greatest value.

; (b)
; Both procedures visit each tree node exactly once, BUT
; version 2 takes less steps than version 1. The difference
; is caused by how cons and append are used in accumulating the
; result.
; Version 2 accumulates using only cons. Intuitively, it accumulates
; the result from right-to-left, which means that the updated result
; can be evaluated by cons'ing the node value to the result "from the right",
; which is evaluated in constant time. Thus, the time complexity of
; version 2 is O(n), where n is the number of nodes in the tree.
;
; Version 1 accumulates using both cons and append. Intuitively,
; it accumulates the result from left-to-right:
; - The result from the left branch is evaluated
; - The result from the right branch is evaluated
;   (and cons'd with the node value)
; - The left and right branch results are appended,
;   which requires n/2 steps, where n is the number of child
;   nodes in the tree.
; Let's try to figure out the time complexity more rigorously.
; Let's assume that the tree is balanced and full.
; Let us denote the levels of the tree 1, 2, ..., h.
; Thus, the height of the tree is h.
; Let's consider the time complexity of the append operations
; on level l. There are 2^(l-1) nodes on the level.
; In each node the number of steps taken by the append operation
; is proportional to how many left children the node has.
; The node has ~2^(h - l) left children. Thus,
; the append operations on level l take proportional to
; 2^(l-1)*2^(h-l) = 2^(h-1) steps.
; Summing over all levels, this adds up to h * 2^(h-1) steps.
; Let us denote the number of nodes in the tree with n.
; It's well known that for perfectly balanced binary trees
; n = 2^h - 1 ~= 2^h and
; h ~= log(n).
; Thus, we can write the time complexity of the append operations
; as log(n) * n / 2 ~ log(n) * n.
; Besides the append operations, a constant amount of computation
; takes place in each node. Thus, the time complexity of version 1
; is O(log(n) * n), which is slower than that of version 2.
