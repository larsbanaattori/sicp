#lang sicp
(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)
            (fold-right op init (cdr seq)))))

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(define (reverse sequence)
  (fold-right
   (lambda (head tail) (append tail (list head)))
   nil
   sequence))

(define (reverse-2 sequence)
  (fold-left
   (lambda (accum head)
     (cons head accum))
   nil
   sequence))

(define l (list 1 2 3 4 5))
(reverse l)
(reverse-2 l)
