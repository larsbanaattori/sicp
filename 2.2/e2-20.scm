#lang sicp

(define (reverse items)
  (define (iter head tail)
    (if (null? head)
        tail
        (iter (cdr head) (cons (car head) tail))))
  (iter items nil))

(define (same-parity first . items)
  (define (add? item)
    (= (remainder item 2) (remainder first 2)))
  (define (iter items accum)
    (cond ((null? items)
           accum)
          ((add? (car items))
           (iter (cdr items)
                 (cons (car items) accum)))
          (else
           (iter (cdr items)
                 accum))))
  (reverse (iter items (list first))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
