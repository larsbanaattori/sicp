#lang sicp
(define (compose f g)
  (lambda (x)
    (f (g x))))

; Version that produces a recursive process
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

; Version that produces an iterative process
(define (repeated-iter f n)
  (define (iter i accum)
    (if (> i n)
        accum
        (iter (inc i) (compose accum f))))
  (iter 1 (lambda (x) x)))

(define (square x) (* x x))

((repeated square 2) 5)
((repeated-iter square 2) 5)
