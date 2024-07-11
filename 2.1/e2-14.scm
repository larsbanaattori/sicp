#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

(define (make-center-percentage center percentage)
  (make-interval (* center (- 1 percentage))
                 (* center (+ 1 percentage))))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (percent i)
  (/ (width i)
     (center i)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (< (* (upper-bound y) (lower-bound y)) 0)
      (error "division by zero error - result not defined")
      (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y))))))

; Compare A to A*A/A
; A is 1 pm5%
; A^2/A is 1.01 pm15%
(define A (make-center-percentage 1 0.5))
(define A2 (div-interval (mul-interval A A) A))
(center A2)
(percent A2)

; Lem's example:
; He is right. The centers are ~0.5% off each other
; and the percentage tolerances are 11% and 3% respectively.
(define R1 (make-center-percentage 20 0.05))
(define R2 (make-center-percentage 10 0.02))
(define one (make-center-percentage 1 0.0))
(define Z1 (div-interval (mul-interval R1 R2) (add-interval R1 R2)))
(define Z2 (div-interval one (add-interval (div-interval one R1) (div-interval one R2))))
(center Z1)
(center Z2)
(percent Z1)
(percent Z2)

; Lem is right: the algebraic form of the expression
; has an impact on the resulting interval.
