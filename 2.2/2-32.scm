#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons(car s) x)) rest)))))

(subsets (list 1 2 3))

; Let S = {a_1,...,a_n} be a set of n items.
; The subsets of S can be divided into two sets:
;   1. The subsets that don't contain a_1
;   2. The subsets that do contain a_1.
; Based on this result, procedure subsets runs a recursion where
; the subsets of S are defined as the union (append) of
;   1. The subsets that don't contain a_1
;      = rest := (subsets (cdr s))
;   2. The subsets that do contain a_1
;      = the union of all sets formed by taking a subset
;        that doesn't contain a_1 (= iteration over rest)
;        and adding a_1 to it
; The base case of the recursion is an empty set (nil).
; Each call of subsets reduces the size of set S (thanks to cdr)
; which means that the base case is eventually hit and the procedure
; can be evaluated.
