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

(define (fermat-test a n)
  (= (expmod a n n) a))

(define (full-fermat n)
  (define (iter a)
    (cond ((not (< a n)) 'prime)
          ((fermat-test a n) (iter (inc a)))
          (else 'not-prime)))
  (iter 1))

(define (smallest-divisor n)
  (define (iter a)
    (cond ((> (square a) n) n)
          ((= (remainder n a) 0) a)
          (else (iter (inc a)))))
  (iter 2))

(define (test n)
  (display n)
  (newline)
  (display 'fermat:)
  (newline)
  (display (full-fermat n))
  (newline)
  (display 'smallest-divisor:)
  (newline)
  (display (smallest-divisor n))
  (newline)
  (newline))

(test 561)
(test 1105)
(test 1729)
(test 2465)
(test 2821)
(test 6601)