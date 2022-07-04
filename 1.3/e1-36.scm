#lang sicp
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess n)
    (let ((next (f guess)))
      (display n)
      (display ':)
      (display next)
      (newline)
      (if (good-enough? next guess)
          next
          (try next (inc n)))))
  (display 0)
  (display ':)
  (display first-guess)
  (newline)
  (try first-guess 1))

(define (avg-damp f)
  (lambda (x)
    (average x (f x))))

(define (average a b)
  (/ (+ a b) 2))

(define (f x)
  (/ (log 1000) (log x)))

(fixed-point f 2)
(fixed-point (avg-damp f) 2)

; Without damping: 34 steps
; With damping: 9 steps