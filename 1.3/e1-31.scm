#lang sicp

(define (f i)
  (/ (* (dec i) (inc i))
     (* i i)))

(define (g i)
  (+ i 2))

; (a)
(define (product term a next b)
  (if (> a b)
      1.0
      (* (term a) (product term (next a) next b))))

(- (product f 3 g 10000) 0.7853981634)

; (b)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1.0))

(- (product-iter f 3 g 10000) 0.7853981634)
(- (product-iter f 3 g 1000000) 0.7853981634)
(- (product-iter f 3 g 100000000) 0.7853981634)
