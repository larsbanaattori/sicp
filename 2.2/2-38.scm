#lang sicp

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)); 3/2 <- 2/3 <- 3/1 <- 1
(fold-left  / 1 (list 1 2 3)); 1 -> 1/1 -> 1/2 -> 1/2/3 = 1/6
(fold-right list nil (list 1 2 3)) ; (1 (2 (3 ()))) <- (2 (3 ()))  <- (3 ()) <- ()
(fold-left list nil (list 1 2 3)) ; () -> (() 1) -> ((() 1) 2) -> (((() 1) 2) 3)

; If the operation is associative, i.e. (op (op a b) c) = (op a (op b c)),
; and commutative with respect to the initial value I, i.e. (op I a) = (op a I),
; left-fold and right-fold yield the same results. Intuitively, the associative property
; means that the order of evaluating the operations doesn't matter.
;
; Here's a blueprint for a more formal proof. Let us denote (op a b) = a ? b.
; Let's assume that ? is associative over a sequence of n items. Then,
;   a_1 ? (a_2 ? ...(a_n-1 ? (a_n ? I)))     ("right-fold")
; = a_1 ? ((a_2 ? ... ? a_n-1 ? a_n) ? I)    (utilizing the commutative property over n items)
; = (a_1 ? (a_2 ? ... ? a_n)) ? I            (utilizing a ? (b ? c) = (a ? b) ? c)
; = (a_1 ? a_2 ? ... ? a_n) ? I              (once again, commutative property over n items)
; = I ? (a_1 ? a_2 ? ... ? a_n)              (commutative with respect to I)
; = (I ? a_1 ? ... ? a_n-1) ? a_n            (utilizing a ? (b ? c) = (a ? b) ? c)
; = ((I ? a_1) ? ... ? a_n-1) ? a_n          ("left-fold")
; that is, left-fold and right-fold are equivalent for a sequence of any length.

; Some examples of associative operations: +, *, lcm (least common multiple)
; and related initial values with respect to which the operation is commutative:
; +, *, gcd, lcm (least common multiple)
(define l (list 1 2 3 4 5))
(newline)
(fold-right + 0 l)
(fold-left + 0 l)
(fold-right * 1 l)
(fold-left * 1 l)
(fold-right lcm 1 l)
(fold-left lcm 1 l)
(fold-right gcd 0 (list 125 90 65 10000015))
(fold-left gcd 0 (list 125 90 65 10000015))

; Some examples of non-associative operations: -, /, sum of squares
(newline)
(define (square x) (* x x))
(define (sos x y) (+ (square x) (square y)))
(fold-right - 0 l)
(fold-left - 0 l)
(fold-right / 1 l)
(fold-left / 1 l)
(fold-right sos 0 l)
(fold-left sos 0 l)
