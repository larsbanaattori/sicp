#lang sicp

; Let T: R^2 -> R^2 be the transformation
; a -> (a + b)q + ap
; b -> bp + aq
; Then T^2 is
; a -> ((a + b)q + ap + bp + aq)q + ((a + b)q + ap)p = a(2q^2 + 2pq + p^2) + b(q^2 + 2pq) = (a + b)(q^2 + 2pq) + a(q^2 + p^2)
; b -> (bp + aq)p + ((a + b)q + ap)q = b(q^2 + p^2) + a(q^2 + 2pq)
; Thus, T^2 is equal to
; a -> (a + b)q' + ap'
; b -> bp' + aq',
; where
; p' = p^2 + q^2
; q' = 2pq + q^2

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (square x)
  (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (test n truth)
  (if (= (fib n) truth)
      (display 'ok)
      (display 'error))
  (newline))

(test 1 1)
(test 10 55)
(test 20 6765)
(test 0 0)
(test 7 13)
(test 13 233)
(test 100 354224848179261915075)