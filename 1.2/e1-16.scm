#lang sicp
(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (dec n)))))
  (iter 1 b n))

(define (square x)
  (* x x))

(define (even? x)
  (= (remainder x 2) 0))

; Some tests
(fast-expt 3 4) ;81
(fast-expt 2 16) ; 65536

; Why does it work?
; Quantity a * b^n remains invariant between iterations
; - If n is even, we iterate a * b^n -> a * (b^2)^(n/2)
; - If n is odd, we iterate a * b^n -> (a*b) -> b^(n-1)
; - The iteration starts from the right quantity: 1 * b^n
; - The iteration ends once n = 0 and returns the right answer:
;    a * b^0 = a
;
; Why is the process iterative?
; - The state of the process is summarized by state variables a, b and n
;    at each point of iteration. The recursive calls don't leave any overhead
;
; Example of how it works (the squaring operations are not shown explicitly)
; (fast-expt 2 13)
; (iter 1 2 13)
; (iter 2 2 12)
; (iter 2 4 6)
; (iter 2 16 3)
; (iter 32 16 2)
; (iter 32 256 1)
; (iter 8192 256 0)
; 8192
