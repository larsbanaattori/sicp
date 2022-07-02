#lang sicp
(define (expmod base exp m)
  (define (squaremod x y)
    (if (and (> x 1)
             (< x (- y 1))
             (= (remainder (square x) y) 1))
        0
        (remainder (square x) y)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (squaremod (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (dec exp) m)) m))))

(define (square x)
  (* x x))

(define (miller-rabin n)
  (define (try-it a)
    (= (expmod a (dec n) n) 1))
  (try-it (+ 1 (random (dec n)))))

(define (prime? n)
  (define (iter count)
    (cond ((> count 100)
           true)
          ((miller-rabin n)
           (iter (inc count)))
          (else false)))
  (iter 1))

; Should be primes
(prime? 11)
(prime? 1579)
(prime? 103837)
(prime? 5002189)
(prime? 50003483)
(prime? 500003599)
(newline)

; Some Carmichael numbers
(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
(newline)

; Some more non-primes
(prime? 74748351)
(prime? 736583)