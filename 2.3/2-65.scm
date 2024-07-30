#lang sicp

; Tree abstraction
(define make-tree list)
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

; O(n)
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

; O(n)
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; Both union-set and intersection-set are O(n) since
; the iter processes are O(n) as proven in the book,
; list->tree/tree->list used to pack/unpack the result/original
; trees are O(n), and these procedures are applied in series.
; Effectively we use list->tree/tree->list to move between the
; binary tree and ordered list representations and use the
; same procedures as in case of the ordered list representation
; to do the union/intersection operations in O(n) time
(define (union-set s1 s2)
  (define (iter l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else
           (let ((x1 (car l1)) (x2 (car l2)))
             (cond ((= x1 x2)
                    (cons x1 (iter (cdr l1) (cdr l2))))
                   ((< x1 x2)
                    (cons x1 (iter (cdr l1) l2)))
                   ((> x1 x2)
                    (cons x2 (iter l1 (cdr l2)))))))))
  (list->tree (iter (tree->list s1) (tree->list s2))))

(define (intersection-set s1 s2)
  (define (iter l1 l2)
    (if (or (null? l1) (null? l2))
        '()
        (let ((x1 (car l1)) (x2 (car l2)))
          (cond ((= x1 x2)
                 (cons x1 (iter (cdr l1) (cdr l2))))
                ((< x1 x2)
                 (iter (cdr l1) l2))
                ((> x1 x2)
                 (iter l1 (cdr l2)))))))
  (list->tree (iter (tree->list s1) (tree->list s2))))

; Testing
(define list1 '(1 3 5 7))
(define list2 '(2 4 6))
(define list3 '(1 2 3 4 5 6 7))

(define set1 (list->tree list1))
(define set2 (list->tree list2))
(define set3 (list->tree list3))

(tree->list (union-set set1 set2))
(tree->list (union-set set1 set3))
(tree->list (union-set set2 set2))
(newline)

(tree->list (intersection-set set1 set2))
(tree->list (intersection-set set1 set3))
(tree->list (intersection-set set3 (list->tree '(1 7))))
