#lang sicp

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

; f(n) = A(0,n) = 2*n
; g(n) = A(1,n) = A(0,A(1,n-1)) = 2*A(1,n-1) = 2*A(0,A(1,n-2)) = 2*2*A(1,n-2)
;               = ... = 2^(n-1)*A(1,1) = 2^(n-1)*2 = 2^n
; works also for base cases g(0) and g(1)
(newline)
(g 2)
(g 3)
(g 10)

; h(n) = A(2,n) = A(1,A(2,n-1)) = 2^h(n-1) = 2^2^h(n-2) = ...
;               = 2^2^....^2 * h(1) = 2^2^....^2 n times
; works also for base cases h(0) and h(1)
(newline)
(h 2) ; 4
(h 3) ; 16
(h 4) ; 65536
(h 5) ; 2^65536 = massive number
