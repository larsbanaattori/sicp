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

; First order Taylor polynomial for f(x, y) = x * y evaluated at (x0, y0)
; f(x0 + dx, y0 + dy)
; ~= f(x0, y0) + (d/dx)f * dx + (d/dy)f * dy
;  = x0*y0 + y0*dx + x0*dy
;  = x0*y0 + (x0*y0/x0)*dx + (x0*y0/y0)*dx
;  = x0*y0*(1 + dx/x0 + dy/y0)
;
; Note that dx/x0 is the relative change of x and
; dy/y0 is the relative change of y.
;
; Thus, we can approximate the interval resulting from multiplying
; intervals x and y with an interval centered at the product of
; the center of x and the center of y and % tolerance which is the
; sum of % tolerances of x and y.

(define (mul-interval-approx x y)
  (make-center-percentage (* (center x) (center y))
                          (+ (percent x) (percent y))))

(define x (make-center-percentage 28 0.01))
(define y (make-center-percentage 44.9 0.02))
(mul-interval x y)
(mul-interval-approx x y)
