#lang sicp
; Tree abstraction
(define make-tree list)
(define (key tree)
  (car (car tree)))
(define (value tree)
  (cadr (car tree)))
(define left-branch cadr)
(define right-branch caddr)

; O(n) implementation of list->tree
(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

; O(log(n)) implementation of lookup
(define (lookup given-key tree)
  (if (null? tree)
      false
      (let ((node-key (key tree))
            (node-value (value tree)))
        (cond ((equal? given-key node-key)
               node-value)
              ((> node-key given-key)
               (lookup given-key (left-branch tree)))
              ((< node-key given-key)
               (lookup given-key (right-branch tree)))))))

; Testing
(define tree (list->tree '((0 Lauri) (1 Ari) (2 Joonas) (3 Kalle) (4 Veikko)
                                     (5 Sami) (6 Joona) (7 Janne) (8 Jorma))))
(lookup 0 tree) ; Lauri
(lookup 4 tree) ; Veikko
(lookup 8 tree) ; Jorma
(lookup 9 tree) ; false
(lookup -1 tree) ; false
