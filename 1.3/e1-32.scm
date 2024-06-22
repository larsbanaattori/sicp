#lang sicp

; (a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; Test for sum

(define (identity x) x)

(sum identity 1 inc 10)

; Test from ex 1.31 for product
(define (f i)
  (/ (* (dec i) (inc i))
     (* i i)))

(define (g i)
  (+ i 2))

(- (product f 3 g 10000) 0.7853981634)

; (b)

(define (accum combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum2 term a next b)
  (accum + 0 term a next b))

(define (product2 term a next b)
  (accum * 1 term a next b))

(sum2 identity 1 inc 10)

(- (product2 f 3 g 10000) 0.7853981634)
