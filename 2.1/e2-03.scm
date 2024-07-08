#lang sicp

(define (rectangle-area rec)
  (* (rectangle-width rec)
     (rectangle-length rec)))

(define (rectangle-perimeter rec)
  (+ (* 2 (rectangle-width rec))
     (* 2 (rectangle-length rec))))

; Rectangles: constructor and selectors

; Version 1: use points
;(define (make-rectangle p1 p2 p3)
;  (cons p1 (cons p2 p3)))
;
;(define (rectangle-width rec)
;  (distance-points (car rec) (car (cdr rec))))
;
;(define (rectangle-length rec)
;  (distance-points (car rec) (cdr (cdr rec))))

; Version 2: use segments
(define (make-rectangle s1 s2)
  (cons s1 s2))

(define (rectangle-width rec)
  (segment-length (car rec)))

(define (rectangle-length rec)
  (segment-length (cdr rec)))

; Segments: constructors and selectors and a bit else
(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (segment-length s)
  (distance-points (start-segment s) (end-segment s)))

; Points: constructors, selectors and a bit else
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (distance-points p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))

(define (square x) (* x x))

; Testing

; Version 1
;(define rec (make-rectangle (make-point 0 0)
;                            (make-point 5 0)
;                            (make-point 0 10)))

; Version 2
(define rec (make-rectangle (make-segment (make-point 0 0)
                                          (make-point 5 0))
                            (make-segment (make-point 0 10)
                                          (make-point 0 0))))

(rectangle-area rec)

(rectangle-perimeter rec)
