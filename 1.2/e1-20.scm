#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define r remainder)

; Normal order (x = remainder is evaluated)
(gcd 206 40)
(gcd 40 (r 206 40)) ; x (1)
(gcd (r 206 40) (r 40 (r 206 40))) ; xx (2)
(gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) ; xxxx (4)
(gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))) ; xxxxxxx (7)
(r (r 206 40) (r 40 (r 206 40))) ; xxxx (4)
2
; -> remainder is evaluated 1+2+4+7+4=18 times in total

; Applicative order
(gcd 206 40)
(gcd 40 (r 206 40)) ; x
(gcd 40 6)
(gcd 6 (r 40 6)) ; x
(gcd 6 4)
(gcd 4 (r 6 4)) ; x
(gcd 4 2)
(gcd 2 (r 4 2)) ; x
(gcd 2 0)
2
; -> remainder is evaluated 4 times in total
