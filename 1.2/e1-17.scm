#lang sicp
(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (* (double a) (halve b)))
        (else (+ a (* a (dec b))))))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (test a b truth)
  (if (= (* a b) truth)
      (display 'OK)
      (display 'ERROR))
  (newline))

; Some tests
(test 2 2 4)
(test 2 3 6)
(test 123 456 56088)
(test 987 123456789 121851850743)
(test -20 11 -220)
(test 100 0 0)
(test 0 100 0)