#lang sicp

; DERIV
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

; GENERIC HELPERS
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define variable? symbol?)

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-sym sym sequence)
  (accumulate
   (lambda (head tail)
     (+ (if (eq? head sym) 1 0) tail))
   0
   sequence))

(define (find x sequence)
  (define (iter i sequence)
    (cond ((null? sequence) -1)
          ((eq? x (car sequence)) i)
          (else (iter (inc i) (cdr sequence)))))
  (iter 0 sequence))

(define (list-head sequence k)
  (define (iter i result sequence)
    (if (= i k)
        result
        (iter (inc i)
              (append result (list (car sequence)))
              (cdr sequence))))
  (iter 0 nil sequence))

(define (list-tail sequence k)
  (define (iter i sequence)
    (if (= i k)
        sequence
        (iter (inc i)
              (cdr sequence))))
  (iter 0 sequence))

; SUM
(define (sum? exp)
  (> (find '+ exp) 0))

(define (addend exp)
  (let ((result (list-head exp (find '+ exp))))
    (if (null? (cdr result))
        (car result)
        result)))

(define (augend exp)
  (let ((result (list-tail exp (inc (find '+ exp)))))
    (if (null? (cdr result))
        (car result)
        result)))

(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        (else (list x '+ y))))

; PRODUCT
(define (product? exp)
  (eq? '* (cadr exp)))

(define multiplier car)

(define (multiplicand exp)
  (let ((result (cddr exp)))
    (if (null? (cdr result))
        (car result)
        result)))

(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else (list x '* y))))

(deriv '(x + 3 * (x + y + 2)) 'x) ; 4
(deriv '(x + 3 * (x + y + 2)) 'y) ; 3
(deriv '(x + 3 * (x + y + 2)) 'z) ; 0
(newline)

(deriv '(x + x * (2 + y)) 'x) ; 1 + 2 + y
(deriv '(x + x * (2 + y)) 'y) ; x

(deriv '(x * x * y + x + a * b * x + z) 'x) ; 2xy + 1 + ab