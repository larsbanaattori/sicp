#lang sicp

(define (pascal row col)
  (if (or (= col 1) (= row col))
      1
      (+ (pascal (dec row) col) (pascal (dec row) (dec col)))))

(define (check row col answer)
  (if (= (pascal row col) answer)
      (display 'OK)
      (display 'ERROR))
  (newline))

(check 1 1 1)
(check 2 1 1)
(check 2 2 1)
(check 5 2 4)
(check 5 3 6)
(check 5 4 4)
