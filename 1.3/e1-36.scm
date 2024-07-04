#lang sicp

(define tolerance 1e-5)

(define (id x) x)

(define (avg-damp f) (lambda (x) (/ (+ x (f x)) 2)))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess i)
    (let ((next (f guess)))
      (display (list next i))
      (newline)
      (if (close-enough? guess next)
          next
          (try next (inc i)))))
  (try first-guess 1))

(define (f x) (/ (log 1000) (log x)))

(fixed-point f 2.0) ; 34 iterations
(newline)
(fixed-point (avg-damp f) 2.0) ; 9 iterations
