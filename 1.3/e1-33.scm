#lang sicp
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a accum)
    (cond ((> a b) accum)
          ((not (filter a)) (iter (next a) accum))
          (else (iter (next a) (combiner accum (term a))))))
  (iter a null-value))

(define (prime? n)
  (= (gd n) n))

(define (gd n)
  (define (iter a)
    (cond ((> (square a) n) n)
          ((= (remainder n a) 0) a)
          (else (iter (inc a)))))
  (iter 2))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (square x) (* x x))

(define (prime-ss a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (rel-prime-prod n)
  (filtered-accumulate *
                       1
                       (lambda (x) x)
                       2
                       inc
                       n
                       (lambda (x) (= (gcd x n) 1))))

; Tests

; Sum of squares of primes from 2 to 12
; 2^2 + 3^2 + 5^2 + 7^2 + 11^2 = 4 + 9 + 25 + 49 + 121 = 208
(prime-ss 2 12)

; (rel-prime-prod 10) = 1 * 3 * 7 * 9 = 189
(rel-prime-prod 10)
