#lang sicp

; Vectors
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

; Procedures on vectors
(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect v2 -1)))

; Tests
(define v1 (make-vect 2 3))
(define v2 (make-vect 3 2))
(xcor-vect v1) ; 2
(ycor-vect v1) ; 3
(add-vect v1 v2) ; (5 5)
(sub-vect v1 v2) ; (-1 1)
(scale-vect v1 2) ; (4 6)
