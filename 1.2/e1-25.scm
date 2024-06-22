#lang sicp
(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (dec n)))))
  (iter 1 b n))

(define (square x)
  (* x x))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (expmod-fast base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod-fast base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod-fast base (dec exp) m))
          m))))

(define (time f base exp m)
  (start-test f base exp m (runtime)))

(define (start-test f base exp m start-time)
  (f base exp m)
  (print-time (- (runtime) start-time)))

(define (print-time elapsed-time)
  (display elapsed-time)
  (newline))

(define (test a n)
  (if (= (expmod a n n) (expmod-fast a n n))
      (display 'match)
      (display 'error))
  (newline)
  (time expmod a n n)
  (time expmod-fast a n n))

(test 50000 100003)
(newline)
(test 500039 1000039)

; Alyssa's idea doesn't work since the modulo calculation gets very slow
; once the numbers get large. In contrast, the fast expmod procedure is fast
; becase it evaluates the modulo operations along the way using relatively
; small numbers.

; Example: (expmod 50000 100003 100003) (Alyssa's version) takes ~63,000x longer
; to evaluate than expmod-fast with the same operands