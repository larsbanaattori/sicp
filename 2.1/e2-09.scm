#lang sicp

; let us consider two intervals: x_1 = a_1...b_1 and x_2 = a_2...b_2.
; let us define w_i:= 0.5 * (b_i - a_i) as the width of interval i
; x_1 + x_2 = (a_1 + a_2)...(b_1 + b2)
; -> w_(1+2) = 0.5 * (b_1 + b_2 - a_1 - a_2) = 0.5 * ((b_1 - a_1) + (b_2 - a_2)) = w_1 + w_2
; same holds for x_1 - x_2 since it can be written as x_1 + (-x_2) and the width of -x_2 is
; equal to the width of x_2

; multiplication (and subtraction)
; - Let's assume x, y > 0 and continue using the notation defined above
; - Then, x * y is (x_l * y_l)...(x_u * y_u) and the (double)width of x * y is
;   x_u * y_u - x_l * y_l, which cannot be reduced to a simple function of w_x and w_y
;   due to the "cross-terms" between x and y.
