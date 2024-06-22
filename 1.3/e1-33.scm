#lang sicp

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result
                        (if (filter a) (term a) null-value)))))
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
  (define (filter i) (= (gcd i n) 1))
  (define (identity i) i)
  (filtered-accumulate * 1 identity 1 inc (dec n) filter))

; Tests

; Sum of squares of primes from 2 to 12
; 2^2 + 3^2 + 5^2 + 7^2 + 11^2 = 4 + 9 + 25 + 49 + 121 = 208
(prime-ss 2 12)

; (rel-prime-prod 10) = 1 * 3 * 7 * 9 = 189
(rel-prime-prod 10)
