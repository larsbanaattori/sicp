#lang sicp

; First adder
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

; Simulating (+ 4 5) with substitution model
; I'll skip evaluating the predicate in the if expression
; I'll also skip evaluating the decrement
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
(newline)
; --> This is a recursive process

; Second adder
(define (p a b)
  (if (= a 0)
      b
      (p (dec a) (inc b))))
(p 4 5)

; Simulating (p 4 5) with substitution model
(p 4 5)
(p 3 6)
(p 2 7)
(p 1 8)
(p 0 9)
9
; ---> This is an iterative process