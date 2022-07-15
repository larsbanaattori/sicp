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
; FRAMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Version 1
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;
;(define (origin-frame frame)
;  (car frame))
;
;(define (edge1-frame frame)
;  (car (cdr frame)))
;
;(define (edge2-frame frame)
;  (car (cdr (cdr frame))))

; Version 2
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define origin (make-vect 0 0))
(define edge1 (make-vect 5 1))
(define edge2 (make-vect -1 5))
(define frame (make-frame origin edge1 edge2))
(origin-frame frame)
(edge1-frame frame)
(edge2-frame frame)
