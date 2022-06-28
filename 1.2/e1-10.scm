#lang sicp
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) ; -> 1024
(A 2 4)  ; -> 65536
(A 3 3)  ; -> 65536
(newline)

; (f n) = (A 0 n) =
; 2n
(A 0 100) ; should be 200

; (g n) = (A 1 n) =
; (A 0 (A 1 n-1)) =
; 2 * (g n-1) (based on the result above)
; --> (g n) = 2^n by induction
(A 1 10) ; should be 2^10 = 1024

; (h n) = (A 2 n) =
; (A 1 (A 2 n-1)) =
; 2^(h n-1) (based on the result above)
; --> (h n) = 2^^n (i.e. n'th tetration of 2) by induction
(A 2 3) ; should be 2^2^2 = 16
(A 2 4) ; should be 2^2^2^2 = 65536