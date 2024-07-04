#lang sicp

(define (fixed-point f first-guess)
  (define tolerance 0.0000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (avg-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

(define (n-root-test a n damps)
  (define (f x) (/ a (expt x (dec n))))
  (fixed-point ((repeated avg-damp damps) f) 1.0))

(n-root-test 4 2 1)
(n-root-test 8 3 1)
(n-root-test 16 4 2)
(n-root-test 32 5 2)
(n-root-test 64 6 2)
(n-root-test 128 7 2)
(n-root-test 256 8 3)
(n-root-test 512 9 3)
(n-root-test 1024 10 3)
(n-root-test 2048 11 3)
(n-root-test 4096 12 3)
(n-root-test 8192 13 3)
(n-root-test (expt 2 16) 16 4)
; -> seems like we need floor(log_2(n)) dampings to calculate nth root

; let's try this out
(define (n-root a n)
  (n-root-test a n (max 1 (floor (log n 2)))))

(define (test end)
  (define (iter i)
    (display i)
    (display ":")
    (display (n-root (expt 2 i) i))
    (newline)
    (if (< i end)
        (iter (inc i))))
  (iter 1))

(newline)
(test 50)
; OK
