#lang sicp
; What we need to show is that each integer n = 2^a*3^b (a, b are integers)
; maps to exactly one pair (a, b).
;
; The key thing to notice here is that:
; - 2^a is always an even number, except if a = 0
; - 3^b is always an odd number. This can be shown by induction.
;   If 3^(b-1) is odd, 3^b = 3 * 3^(b-1) is odd as well. Since
;   3 is odd, the claim is proven by induction.
;
; Thus, we can deconstruct n = 2^a*3^b into a and b through the
; following algorithm(s):
; - To get a: divide n by 2 repeatedly until the result is odd.
;   --> a is the number of divisions performed
; - To get b: divide n by 3 repeatedly until the result is not an integer
;   --> b is the number of divisions performed
;
; Since both algorithms are deterministic and terminate, the mapping
; from n to (a,b) is bijective. Thus, pairs of non-negative integers
; can be represented this way.

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car n)
  (if (odd? n)
      0
      (+ 1 (car (/ n 2)))))

(define (cdr n)
  (if (> (remainder n 3) 0)
      0
      (+ 1 (cdr (/ n 3)))))

(define (print-pair a b)
  (display a)
  (display ",")
  (display b)
  (newline))

(define (test a b)
  (display "start: ")
  (print-pair a b)
  (let ((n (cons a b)))
    (let ((aa (car n))
          (bb (cdr n)))
      (display "result:")
      (print-pair aa bb)
      (if (and (= a aa)
               (= b bb))
          (display "OK!")
          (display "ERROR!"))))
  (newline)
  (newline))

(test 0 0)
(test 1 0)
(test 0 1)
(test 1 1)
(test 2 0)
(test 0 2)
(test 31 72)
(test 23 25)
