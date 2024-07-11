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
  (define (interval-sign x)
    (let ((lb (lower-bound x))
          (ub (upper-bound x)))
      (cond ((and (> lb 0) (> ub 0)) 1)
            ((and (< lb 0) (< ub 0)) -1)
            (else 0))))
  (let ((x-sign (interval-sign x))
        (y-sign (interval-sign y))
        (x-lo (lower-bound x))
        (x-up (upper-bound x))
        (y-lo (lower-bound y))
        (y-up (upper-bound y)))
    (cond ((and (= x-sign 1) (= y-sign 1))
           (make-interval (* x-lo y-lo) (* x-up y-up)))
          ((and (= x-sign 1) (= y-sign 0))
           (make-interval (* x-up y-lo) (* x-up y-up)))
          ((and (= x-sign 1) (= y-sign -1))
           (make-interval (* x-up y-lo) (* x-lo y-up)))
          ((and (= x-sign 0) (= y-sign 1))
           (make-interval (* x-lo y-up) (* x-up y-up)))
          ((and (= x-sign 0) (= y-sign 0))
           (make-interval
            (min (* x-up y-lo) (* x-lo y-up))
            (max (* x-up y-up) (* x-lo y-lo))))
          ((and (= x-sign 0) (= y-sign -1))
           (make-interval (* x-up y-lo) (* x-lo y-lo)))
          ((and (= x-sign -1) (= y-sign 1))
           (make-interval (* x-lo y-up) (* x-up y-lo)))
          ((and (= x-sign -1) (= y-sign 0))
           (make-interval (* x-lo y-up) (* x-lo y-lo)))
          (else
           (make-interval (* x-up y-up) (* x-lo y-lo))))))

(define (div-interval x y)
  (if (< (* (upper-bound y) (lower-bound y)) 0)
      (error "division by zero error - result not defined")
      (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y))))))

(define (test x-lo x-up y-lo y-up z-lo z-up)
  (let ((z (mul-interval (make-interval x-lo x-up) (make-interval y-lo y-up))))
    (if (and (= z-lo (lower-bound z)) (= z-up (upper-bound z)))
        (display "OK!")
        (display "ERROR!")))
  (newline))

(test 1 2 1 2 1 4) ; pos and pos
(test 1 2 -10 20 -20 40) ; pos and mixed
(test 2 3 -20 -10 -60 -20) ; pos and neg
(test -2 3 1 2 -4 6) ; mixed and pos
(test -2 3 -20 10 -60 40) ; mixed and mixed
(test -2 3 -20 -10 -60 40) ; mixed and neg
(test -2 -1 2 3 -6 -2) ; neg and pos
(test -2 -1 -2 3 -6 4) ; neg and mixed
(test -2 -1 -3 -2 2 6) ; neg and neg
