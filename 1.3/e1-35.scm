#lang sicp

; Let's substitute x for golden ratio (1 +  sqrt(5)) / 2 in term 1 + 1/x = (x + 1) / x ->
; = (3 + sqrt(5)) / (1 + sqrt(5)) (multiply with 1 - sqrt(5))
; = (3 + sqrt(5) - 3*sqrt(5) - 5) / (1 - 5)
; = - 2 *(sqrt(5) + 1) / - 4
; = (1 + sqrt(5)) / 2
; = golden ratio
; Thus, golden ratio is a fixed point of x -> 1 + 1/x

; Use this fact (^^) to compute phi by means of the fixed-point procedure

(define tolerance 1e-9)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ; close enough