#lang sicp

; Procedure that yields a recursive process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

; Procedure that yields a recursive process
(define (g n)
  (define (iter a b c i)
    (cond ((< i 3) i)
          ((> i n) a)
          (else (iter (+ a (* 2 b) (* 3 c)) a b (inc i)))))
  (iter 2 1 0 3))

; Testing
(f 0) ; 0
(f 1) ; 1
(f 2) ; 2
(f 3) ; 4  = 2 + 2*1 + 3*0
(f 4) ; 11 = 4 + 2*2 + 3*1
(f 5) ; 25 = 11 + 2*4 + 3*2
(f 6) ; 59 = 25 + 2*11 + 3*4

(define (test n)
  (if (= (f n) (g n))
      (display 'OK)
      (display 'ERROR))
  (newline))

(test 3)
(test 4)
(test 5)
(test 6)
(test 7)
