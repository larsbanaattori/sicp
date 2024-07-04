#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

(define (square x) (* x x))

((repeated square 2) 5)

(define (repeated-iter f n)
  (define (iter i accum)
    (if (> i n)
        accum
        (iter (inc i) (compose f accum))))
  (iter 1 (lambda (x) x)))

((repeated-iter square 2) 5)
