#lang sicp

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (iter a b c i)
    (if (> i n)
        a
        (iter (+ a (* 2 b) (* 3 c)) a b (inc i))))
  (if (< n 3)
      n
      (iter 2 1 0 3)))

(define (check n)
  (if (= (f-rec n) (f-iter n))
      (display 'OK)
      (display 'ERROR))
  (newline))

(check 1)
(check 2)
(check 3)
(check 4)
(check 5)
(check 10)
(check 20)

(newline)
(f-rec 35) ; veeery slow
(f-iter 35) ; much faster