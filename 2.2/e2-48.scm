#lang sicp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VECTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LINE SEGMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constructors and selectors
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define start (make-vect 1 1))
(define end (make-vect 5 5))
(define segment (make-segment start end))
(start-segment segment)
(end-segment segment)
