#lang sicp

(define (for-each f items)
  (define (next items)
    (f (car items))
    (for-each f (cdr items)))
  (if (null? items)
      (newline)
      (next items)))

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))

(for-each 
 (lambda (x) (newline) (display x))
 (list 1))

(for-each 
 (lambda (x) (newline) (display x))
 (cons 1 nil))

(for-each 
 (lambda (x) (newline) (display x))
 nil)
