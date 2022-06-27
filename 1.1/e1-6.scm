#lang sicp
; The square-root program using "new-if" produces
; an infinite loop and, thus, crashes eventually.
; This happens because the else-clause (alternative)
; is evaluated every time new-if is evaluated (thanks to
; applicative-order evaluation), no
; matter whether the predicate is true or false.
; Since the else-clause is a recursive call in the
; square-root program, an infinite loop ensues.
; Intuitively, the iteration never stops since it
; continues even if the base case (= a good-enough guess)
; is reached.
;
; The moral of the story seems to be that tail recursion
; works only when the alternative expression of an if
; expression is evaluated contingent on the predicate'
; evaluating to false