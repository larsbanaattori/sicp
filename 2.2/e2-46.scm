#lang sicp

; Constructor and selectors
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

; Procedures on vectors
(define (add-vect v w)
  (make-vect
   (+ (xcor-vect v) (xcor-vect w))
   (+ (ycor-vect v) (ycor-vect w))))

(define (scale-vect v a)
  (make-vect
   (* a (xcor-vect v))
   (* a (ycor-vect v))))

(define (sub-vect v w)
  (add-vect v (scale-vect w -1)))

; Tests
(define v (make-vect 1 2))
(define w (make-vect 3 5))
(add-vect v w)
(scale-vect v 1.5)
(sub-vect v w)
