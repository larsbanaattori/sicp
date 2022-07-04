#lang sicp
(define tolerance 0.000001)

(define (fixed-point f initial-guess)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (good-enough? next guess)
          next
          (try next))))
  (try initial-guess))

(define (average-damp f)
  (lambda (x)
    (/ (+ (f x) x) 2)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

(define (avg-damp-n-times f n)
  ((repeated average-damp n) f))

(define (n-root-test a n damps)
  (define (f x)
    (/ a (expt x (dec n))))
  (fixed-point (avg-damp-n-times f damps) 1.0))

; Some tests to find the pattern
; It seems like the right number of damps is
; floor(log2(n)) for figuring out the n'th root
(n-root-test 4 2 1) ; 2nd root -> 1 damping
(n-root-test 8 3 1) ; 3rd root -> 1 damping
(n-root-test 16 4 2) ; 4th root -> 2 dampings
(n-root-test 32 5 2) ; 5th root -> 2 dampings
(n-root-test 64 6 2) ; 6th root -> 2 dampings
(n-root-test 128 7 2) ; 7th root -> 2 dampings
(n-root-test 256 8 3) ; 8th root -> 3 dampings
(n-root-test 512 9 3) ; 9th root -> 3 dampings
(n-root-test 1024 10 3) ; 10th root -> 3 dampings
(n-root-test 2048 11 3) ; 11th root -> 3 dampings
(n-root-test 4096 12 3) ; 12th root -> 3 dampings
(n-root-test 8192 13 3) ; 13th root -> 3 dampings
(n-root-test 65536 16 4) ; 16th root -> 4 dampings

; Let's define the solution out and try it for roots 2...50 of 2^2...2^50
; The answer should always be ~2
(define (n-root a n)
  (define (f x)
    (/ a (expt x (dec n))))
  (fixed-point (avg-damp-n-times f (floor (log n 2))) 1.0))

(define (test end)
  (define (iter i)
    (display i)
    (display ':)
    (display (n-root (expt 2 i) i))
    (newline)
    (if (< i end)
        (iter (inc i))))
  (iter 2))

(newline)
(test 50)
