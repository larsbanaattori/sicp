#lang sicp

; Still O(n), but potentially worse due to
; set of same cardinality requiring at least
; many elements as in the earlier representation
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 1 '(2 3 2 1 3 2 2))
(element-of-set? 2 '(2 3 2 1 3 2 2))
(element-of-set? 3 '(2 3 2 1 3 2 2))
(element-of-set? 4 '(2 3 2 1 3 2 2))
(newline)

; O(1) since no search is needed
(define (adjoin-set x set)
  (cons x set))

(adjoin-set '4 '(1 2 3 4))
(newline)

; O(n) since append is called
(define (union-set s1 s2)
  (append s1 s2))

; O(n^2) since nothing changes.
; Slower in practice due to duplicates
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))

; Probably one can find application where both
; of the representations work better than the other.
; It's a trade-off between adjoin/union and
; intersection/element-of-set speed
