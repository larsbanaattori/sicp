#lang sicp

; Usage of segments
(define (midpoint-segment s)
  (mid-point (start-segment s)
             (end-segment s)))

; Constructor and selectors for segments
(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

; Additional usage of points
(define (mid-point p1 p2)
  (make-point (average (x-point p1) (x-point p2))
              (average (y-point p1) (y-point p2))))

; Constructor and selectors for points
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

; Utilities
(define (average x y) (/ (+ x y) 2))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Testing
(define s (make-segment (make-point 0 0) (make-point 4 8)))
(print-point (midpoint-segment s))
