#lang sicp

; Segments
(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

; Points
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (mul-point p a)
  (make-point
   (* (x-point p) a)
   (* (y-point p) a)))
(define (add-point p1 p2)
  (make-point
   (+ (x-point p1) (x-point p2))
   (+ (y-point p1) (y-point p2))))

; Procedures
(define (midpoint-segment s)
  (mul-point (add-point (start-segment s) (end-segment s)) 0.5))

; Testing
(define s (make-segment (make-point 0 0) (make-point 4 8)))
(print-point (midpoint-segment s))
