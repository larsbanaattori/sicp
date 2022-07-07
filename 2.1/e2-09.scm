#lang sicp
; sum (and difference)
; - Let the intervals be x_l...x_u -> w_x = (x_u - x_l)/2 and y_l...y_u -> w_y = (y_u - y_l)/2
; - The interval x + y is (x_l + y_l)...(x_u + y_u)
;   The width of x + y is, thus, (x_u + y_u - x_l - y_l) / 2 = w_x + w_y
; - Same holds to the width of x - y, we use the proof above to x + (-y)
;
; multiplication (and subtraction)
; - Let's assume x, y > 0 and continue using the notation defined above
; - Then, x * y is (x_l * y_l)...(x_u * y_u) and the (double)width of x * y is
;   x_u * y_u - x_l * y_l, which cannot be reduced to a simple function of w_x and w_y
;   due to the "cross-terms" between x and y.
; - Intuitively, the absolute values of x and y either dampen or amplify
;   the widths of x and y. Consider an example where y = 100, w_y = 0, x_l = 0.9 and x_u = 1.1
;   the result is 90...110, i.e., the width is w_x * y = w_x * 100
; - This implies that the width of a subtraction isn't a simple function of w_x and w_y
;   since subtraction involves mapping y -> 1/y (which itself puts w_y through a non-linear mapping,
;   the shape of which depends on the absolute values of y_l and y_u) and multiplication x * (1/y)
