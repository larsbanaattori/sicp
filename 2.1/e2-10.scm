#lang sicp

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

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

(define r1 (make-interval 6.12 7.48))
(define r2 (make-interval 4.47 4.94))
(define one (make-interval 1.0 1.0))
(div-interval one
              (add-interval
               (div-interval one r1)
               (div-interval one r2))) ; Test, should be ~2.58-2.97

(sub-interval r1 r2) ; Test, should be 1.18-3.01

(define r3 (make-interval -2 -1))
(define r4 (make-interval -1 1))

(div-interval r1 r3) ; Should work
(div-interval r1 r4) ; Should not work
