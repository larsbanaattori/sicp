#lang sicp

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define (no-more? list)
    (null? list))
  (define (except-first-denomination list)
    (cdr list))
  (define (first-denomination list)
    (car list))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(cc 100 us-coins)
(cc 100 uk-coins)

(cc 100 (reverse us-coins))
(cc 100 (reverse uk-coins))

; Order doesn't matter
; By it's design, cc goes through all possible ways to
; make the change and counts them one by one.
; The result doesn't change no matter in which order
; we go through the possibilities
