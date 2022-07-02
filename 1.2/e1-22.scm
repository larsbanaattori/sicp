#lang sicp
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (inc test-divisor)))))

(define (square x)
  (* x x))

(define (divides? x y)
  (= (remainder y x) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (define (search-iter n end)
    (timed-prime-test n)
    (if (not (> n end))
        (search-iter (+ n 2) end)))
  (if (even? start)
      (search-iter (+ start 1) end)
      (search-iter start end)))

(search-for-primes 1000 1020) ; ~3 microseconds per prime
(newline)
(search-for-primes 10000 10040) ; ~7 microseconds per prime
(newline)
(search-for-primes 100000 100050) ; ~20 microseconds per prime
(newline)
(search-for-primes 1000000 1000050) ; ~60 microseconds per prime
(newline)
(search-for-primes 10000000 10000200) ; ~180 microseconds per prime

; The results are in line with the O(sqrt(n)) time complexity assumption
; Growing the input by a factor of 10^2 increases runtime by ~10^2.
; Also, growing the input by a factor of 10^1 increases runtime by ~ 3 (sqrt(10).
; This is most apparent when comparing cases n = 10^4, 10^5, 10^6 and 10^7