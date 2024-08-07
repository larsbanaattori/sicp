#lang sicp

; Helpers
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b)))) 

; Constructors & selectors
(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (sign (if (< (* n d) 0) - +)))
    (cons (sign (/ (abs n) g)) (/ (abs d) g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

; Arithmetic
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
             (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 8 10))
(print-rat (make-rat -8 10))
(print-rat (make-rat 8 -10))
(print-rat (make-rat -8 -10))
