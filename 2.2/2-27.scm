#lang sicp

(define (reverse items)
  (define (iter tail result)
    (if (null? tail)
        result
        (iter (cdr tail) (cons (car tail) result))))
  (iter items nil))

(define (deep-reverse items)
  (define (iter items result)
    (cond ((null? items)
           result)
          (else
           (let ((head (car items))
                 (tail (cdr items)))
             (if (pair? head)
                 (iter tail (cons (deep-reverse head) result))
                 (iter tail (cons head result)))))))
  (iter items nil))

(define x (list (list 1 2) (list 3 4)))
x
(reverse x)
(deep-reverse x)
