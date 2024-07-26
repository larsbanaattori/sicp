#lang sicp

; Louis' code which adjoins the admissible solutions for k-1
; with the new column:
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position
           new-row k rest-of-queens))
        (queen-cols (- k 1)))
   (enumerate-interval 1 board-size)))

; This is bad because (queen-cols (- k 1)) is evaluated
; k times when evaluating (queen-cols k).
; In contrast, the faster solution calls (queen-cols (- k 1))
; only once when evaluating (queen-cols k).
;
; Let T(n, k) be the running time of Louis' program (queen-cols k)
; relative to the faster implementation when considering a n*n board.
; Let's first consider T(n, 1). In this case the programs are practically
; equally fast since the only additional work done by Louis' program is to
; evaluate the predicate (= 0 0) n times. Thus, T(n, 1) ~= 1
; Let's then consider T(n, 2). Now, the extra work done by Louis' program
; is to evaluate (queen-cols 1) n times instead of just once. Since T(n, 1) = 1,
; T(n, 2) = n.
; Let's now assume that T(n, k-1) = n^(k-2). Then, the extra work done by Louis'
; (queen-cols k) is to evaluate (queen-cols (- k 1)) n times, i.e.,
; T(n, k) = n * n^(k-2) = n^(k-1).
; Thus, by induction, T(n, n) = n^(n-1).
;
; This is assuming that (enumerate-interval 1 board-size) is actually
; evaluated only once in both implementations and ignoring the other
; "overhead" work done in queen-cols than the recursive call(s), i.e.,
; creating new positions and filtering them.
; Thus, the actual time complexity ratio is likely lower.
; The gist is that the fast solution
; evaluates the admissability of each (partial) solution only once, whereas
; Louis' solution repeats that work over and over again.
;
; The point can be made in terms of drawing the recursion tree of queen-cols
; The original solutions leads to a linear recursion:
; (queen-cols n) -> (queen-cols n-1) -> ...
; Louis' solution leads to a tree with branching factor equal to board-size
