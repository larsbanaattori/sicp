#lang sicp
(define (find-divisor n test-divisor next)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor) next))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (square x)
  (* x x))

(define (divides? x y)
  (= (remainder y x) 0))

(define (smallest-divisor n next)
  (find-divisor n 2 next))

(define (prime? n next)
  (= (smallest-divisor n next) n))

(define (timed-prime-test n next)
  (newline)
  (display n)
  (start-prime-test n next (runtime)))

(define (start-prime-test n next start-time)
  (if (prime? n next)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (comparison n)
  (timed-prime-test n inc)
  (timed-prime-test n next)
  (newline))

; ~1.5x evaluation time with inc vs. next
(comparison 1009)
(comparison 1013)
(comparison 1019)
(comparison 1021)

; ~1.3x evaluation time with inc vs. next
(comparison 10007)
(comparison 10009)
(comparison 10037)
(comparison 10039)

; ~1.6x evaluation time with inc vs. next
(comparison 100003)
(comparison 100019)
(comparison 100043)
(comparison 100049)

; ~1.6x evaluation time with inc vs. next
(comparison 1000003)
(comparison 1000033)
(comparison 1000037)
(comparison 1000039)

; ~1.6x evaluation time with inc vs. next
(comparison 10000019)
(comparison 10000079)
(comparison 10000103)
(comparison 10000121)

; Overall, it seems like procedure prime? is "only" ~ 1.5x slower when
; using inc to evaluate the next iteration instead of next.
; Why is this the case instead of 2.0x?
; One possible reason is that next is slower to evaluate than
; inc - it does contain expression (= n 2) as the predicate of
; the if expression. Thus, this added overhead is evaluated each time
; next is evaluated. I cannot think of other reasons - the process resulting
; from evaluating prime? is otherwise identical.