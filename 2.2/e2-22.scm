#lang sicp
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square x) (* x x))

(square-list (list 1 2 3 4 5))

; Procedure above produces the right values, but in the reverse order.
; The order is reversed because the accumulator (answer) is iteratively
; appended to the next item to be added to it, not the other way around.

(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list-2 (list 1 2 3 4 5))

; Now the answer is wrong (again) since the accumulator is "nested"
; as the first value of the new pair generated. This leads to the initial
; value of the accumulator (nil) being nested into pair (nil, 1), which then
; gets nested into pair ((nil, 1), 4), which gets nested into pair
; (((nil, 1), 4), 9)...
; The iteration doesn't produce a proper list since the cons call should
; get a numeral as its first argument and a list as the second argument,
; not the other way around as is done now.
