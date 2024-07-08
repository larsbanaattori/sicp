#lang sicp

; Since 2 and 3 are primes, Prime Factorization Theorem (PFT) states that
; for each number i = 2^a * 3^b, i > 1,
; the pair (a, b) corresponds to a unique value of i.
; Otherwise we'd contradict PFT, i.e., we'd have two disctinct ways to factor
; i with respect to a and b.
; Furthermore, i = 1 if and only if a = b = 0.
; Thus, mapping (a, b) -> 2^a * 3^b is bijective and, hence, we can use it
; as a way to represent pairs of non-negative integers.

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car p)
  (if (odd? p)
      0
      (+ 1 (car (/ p 2)))))

(define (cdr p)
  (if (> (remainder p 3) 0)
      0
      (+ 1 (cdr (/ p 3)))))

(define (print-pair a b)
  (display a)
  (display ",")
  (display b)
  (newline))

(define (test a b)
  (display "start: ")
  (print-pair a b)
  (let ((n (cons a b)))
    (let ((aa (car n))
          (bb (cdr n)))
      (display "result:")
      (print-pair aa bb)
      (if (and (= a aa)
               (= b bb))
          (display "OK!")
          (display "ERROR!"))))
  (newline)
  (newline))

(test 0 0)
(test 1 0)
(test 0 1)
(test 1 1)
(test 2 0)
(test 0 2)
(test 31 72)
(test 23 25)
