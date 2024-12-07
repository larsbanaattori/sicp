#lang sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; (1)
; Procedure above uses the data-directed dispatcing trick to
; find the right derivative procedure for expressions which are
; not numbers or symbols.
;
; We cannot assimilate number? and variable? into the data-directed dispatch
; because we are using primitive number and symbol representations
; for numbers and variables. Those representations don't contain
; a tag in the same vay as expressions.
;
; (2)
(define (install-deriv-sum-prod)
  (define (deriv-sum operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cdr operands) var)))
  (define (deriv-prod operands var)
    (make-sum
     (make-product (car operands)
                   (deriv (cadr operands) var))
     (make-product (deriv (car operands) var)
                   (cadr operands))))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-prod))

; (3)
; Very much like above. Additive wow.

; (4)
; We'd just need to make sure the install packages
; work correctly. If arguments in procedure put would also
; be reindexed, we'd need to reindex the put calls in
; install packages appropriately.
