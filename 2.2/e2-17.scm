#lang sicp
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(last-pair (list nil))
(last-pair (list 1))
(last-pair (list 23 72 149 34))
