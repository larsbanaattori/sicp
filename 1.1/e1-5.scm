#lang sicp
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; What happens here in applicative- and
; normal-order evaluation?
(test 0 (p))

; Applicative-order evaluation tries to evaluate the
; operands before evaluating the test procedure
; using the substitution model. This implies that
; combination (p) is attepted to be evaluated.
; Since procedure p is an infinite loop, the program
; will not terminate.

; Normal-order evaluation returns 0 and avoids the
; infinite loop. The trick is that the combination
; (test 0 (p)) is expanded to the if expression
; (if (= 0 0) 0 (p)). According to the evaluation rule
; of special form if, the predicate (= 0 0) is evaluated
; first. Since it evaluates to #t, the if expression
; evaluates to the consequent, i.e., 0. Notably,
; the alternative (p) is not evaluated.