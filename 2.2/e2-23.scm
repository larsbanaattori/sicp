#lang sicp
(define (for-each proc items)
  (cond ((null? items)
         (newline))
        (else
         (proc (car items))
         (for-each proc (cdr items)))))

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
