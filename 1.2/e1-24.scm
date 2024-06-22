#lang sicp
(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (min 4294967087 (- n 1))))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 10))

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

; n ~= 10^3
(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 1021)

; n ~= 10^4
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 10039)

; n ~= 10^5
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 100049)

; n ~= 10^6
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)
(timed-prime-test 1000039)

; n ~= 10^8
(timed-prime-test 100000007)
(timed-prime-test 100000217)
(timed-prime-test 100000399)
(timed-prime-test 100000567)

; n ~= 10^10
(timed-prime-test 10000000019)
(timed-prime-test 10000000259)
(timed-prime-test 10000000583)
(timed-prime-test 10000001437)

; n ~= 10^12
(timed-prime-test 1000000005721)
(timed-prime-test 1000000005713)
(timed-prime-test 1000000005709)
(timed-prime-test 1000000005707)

; One would expect the running time to double between n=1,000 and n=1,000,000 since
; the time complexity is O(log(n)).
; However, this doesn't seem to happen: the time it takes to evaluate fast-prime?
; for input n ~= 10^6 is is only ~1.2x compared to n ~= 10^3.
; My guess is that this happens because there is some fixed overhead to be evaluated
; when running fast-prime?:
; - generating the random numbers
; - the procedures doing the timing (timed-prime-test, report-prime-elapsed-time...)
; - the iteration overhead in fermat-test
; expmod is probably so fast (with the relative small inputs tested) that it's time
; complexity gets lost in the overhead that needs to be executed despite input size.

; However, when running fast-prime? with larger n, results seem to support logarithmic
; time complexity (see the last tests above):
; - n ~= 10^8 vs n ~= 10^4 -> ~1.8x
; - n ~= 10^10 vs n ~= 10^5 -> ~2.4x
; - n ~= 10^12 vs. n ~= 10^6 -> ~ 3.3x
; The growth rate actually increases with n. This is probably because the arithmetic
; operations (multiplication, modulo) get slower as n increases.