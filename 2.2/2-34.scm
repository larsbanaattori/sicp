#lang sicp

(define (accumulate op nil seq)
  (if (null? seq)
      nil
      (op (car seq) (accumulate op nil (cdr seq)))))

(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1)) ; 79
(horner-eval -1 (list 1 3 0 5 0 1)) ; -8
