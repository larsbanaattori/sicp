#lang sicp
(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (sign (if (< (* n d) 0)
                  -
                  +)))
    (cons (sign (/ (abs n) g))
          (/ (abs d) g))))

(define numer car)

(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 8 10))
(print-rat (make-rat -8 10))
(print-rat (make-rat 8 -10))
(print-rat (make-rat -8 -10))
