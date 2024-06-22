#lang sicp

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

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? x) (= (smallest-divisor x) x))

(define (search-for-primes a b)
  (define (iter i)
    (cond ((<= i b)
           (timed-prime-test i)
           (iter (inc i)))))
  (iter a))

(search-for-primes 1000 1019) ; 2-3 units of time
(newline)
(search-for-primes 10000 10037) ; 5-6 units of time
(newline)
(search-for-primes 100000 100043) ; 16-17 units of time
(newline)
(search-for-primes 1000000 1000037) ; 50-51 units of time
(newline)
(search-for-primes 10000000 10000103) ; 150-152 units of time

; The O(sqrt(n)) prediction holds true quite well after n = 10^4