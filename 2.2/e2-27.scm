#lang sicp
(define (reverse items)
  (define (iter head tail)
    (if (null? head)
        tail
        (iter (cdr head) (cons (car head) tail))))
  (iter items nil))

(define (deep-reverse items)
  (define (iter head tail)
    (cond ((null? head)
           tail)
          ((pair? (car head))
           (iter (cdr head) (cons (iter (car head) nil) tail)))
          (else
           (iter (cdr head) (cons (car head) tail)))))
  (iter items nil))

(define (test items)
  (display items)
  (newline)
  (display (reverse items))
  (newline)
  (display (deep-reverse items))
  (newline)
  (newline))

(define x
  (list (list 1 2) (list 3 4)))

(define y
  (list 1 2 3 4))

(test x)
(test y)
(test (list 1))
(test (list 1 2))
(test (list (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8 9))))
