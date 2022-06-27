; Explanation of the problems with the original procedure
;
; *** Problem with large inputs ***
; The procedure doesn't converge with large inputs
; (try (sqrt 10e20) for example). The reason is related
; to the limited accuracy of floating point numbers.
; With sufficiently large inputs, the Scheme implementation
; cannot store guesses with such accuracy that the square
; of the guess would be close enough to x in absolute terms.
; Thus, the iteration ends up in an infinite loop where the
; guess isn't improved, i.e., a fixed point is reached given
; the accuracy with which floating point numbers can be
; represented, but the fixed point is not accurate enough to
; satisfy the termination predicate good-enough?. To be
; precise, the problem occurs in procedure improve:
; although the procedure should always calculate an improved
; guess in mathematical terms, this doesn't happen in
; practice if the difference between the current guess
; and the updated one is less than the accuracy of the
; floating point number representation. In such situation,
; rounding ensures that the guess doesn't change.
;
; *** Problem with small inputs ***
; With small inputs (try (sqrt 10e-9) for example)
; the problem is that an absolute error of 10e-3
; corresponds to a huge relative error (~10e-6 in case
; of (sqrt 10e-9).
;
; Here's an improved version that terminates when the
; guess changes by less than 10e-9 in relative terms.
;
; The improved version seems to work well: the relative
; errors ot the output (as defined below) are below 10e-9.
;
; Another possible way to improve the algorithm would be to
; define the termination criterion on the basis of the relative
; error of the square of the guessed square root.

#lang sicp
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (/ (abs (change guess x)) guess) 10e-9))

(define (change guess x)
  (- guess (improve guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (abs-error x)
  (abs (- x (square (sqrt x)))))

(define (square x) (* x x))

(define (rel-error x)
  (/ (abs-error x) x))

(define (test x)
  (display (abs-error x))
  (newline)
  (display (rel-error x))
  (newline)
  (newline))

; Test 1: sqrt(3)
; abs-error = absolute error of the square of
;             evaluated square-root
; rel-error = relative error of the square of
;             evaluated square root
(test 3)

; Test 2: sqrt(10e30)
(test 10e30)

; Test 3: sqrt(10e-30)
(test 10e-9)