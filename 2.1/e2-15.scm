; Eva is right in Lem's example.
; I suppose she is right in the general case too.
; Intuitively, repeating the same variable means that
; the uncertainty from that variable is duplicated.
; The problem in our interval math seems to be that
; we assume the different instances of a variable in
; an expression to be independent in terms of uncertainty
; Consider X - X as an example. In reality, X - X is always
; zero. Our program thinks that the result is a (zero-centered)
; interval with the width double the width of X.
; A similar result applies to X / X.
; Thus, I buy Alyssa's claim that the tightest intervals are
; produced by forms in which variables are not repeated.
;
; I don't want to go too deep into this exercise as it would require
; further study of interval arithmetic (which is a new topic to me).
