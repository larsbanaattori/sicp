#lang sicp
(define us-coins 
  (list 50 25 10 5 1))

(define us-coins-messed
  (list 1 50 5 25 10))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

(define uk-coins-messed
  (list 0.5 100 1 50 2 20 5 10))

(define (reverse items)
  (define (iter head tail)
    (if (null? head)
        tail
        (iter (cdr head) (cons (car head) tail))))
  (iter items nil))

(define us-coins-reversed (reverse us-coins))
(define uk-coins-reversed (reverse uk-coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(cc 100 us-coins)
(cc 100 us-coins-reversed)
(cc 100 us-coins-messed)

(cc 100 uk-coins)
(cc 100 uk-coins-reversed)
(cc 100 uk-coins-messed)

; The order of list coin-values doesn't matter.
; The reason is that the algorithm goes through (and counts) all
; possible ways to make change. Thus, the order of trying or eliminating
; coins of different denominations doesn't matter.
