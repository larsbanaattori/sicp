#lang sicp

; DERIV
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation
            (base exp)
            (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? exp) (symbol? exp))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; GENERIC HELPERS
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (contains? sym exp) (memq sym exp))

(define (items-before sym exp)
  (if (or (null? exp) (eq? (car exp) sym))
      '()
      (cons (car exp) (items-before sym (cdr exp)))))

(define (exp-before sym exp)
  (items->exp (items-before sym exp)))

(define (items-after sym exp)
  (cond ((null? exp) '())
        ((eq? (car exp) sym) (cdr exp))
        (else (items-after sym (cdr exp)))))

(define (exp-after sym exp)
  (items->exp (items-after sym exp)))

(define (items->exp items)
  (if (null? (cdr items))
      (car items)
      items))

(define (operation exp)
  (cond ((not (pair? exp))
         'no-op)
        ((contains? '+ exp)
         '+)
        ((contains? '* exp)
         '*)
        ((contains? '** exp)
         '**)
        (else
         (error "unknown operation type: OPERATION" exp))))

(define (=number? x num)
  (and (number? x) (= x num)))

; SUM
(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        (else (list x '+ y))))
(define (sum? x) (eq? (operation x) '+))
(define (addend x) (exp-before '+ x))
(define (augend x) (exp-after '+ x))

; PRODUCT
(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((and (number? x) (number? y)) (* x y))
        ((=number? x 1) y)
        ((=number? y 1) x)
        (else (list x '* y))))
(define (product? x) (eq? (operation x) '*))
(define (multiplier x) (exp-before '* x))
(define (multiplicand x) (exp-after '* x))

; EXPONENTIATION
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 0) 0)
        ((and (number? base) (number? exponent))
         (expt base exponent))
        (else
         (list base '** exponent))))
(define (exponentiation? x) (eq? (operation x) '**))
(define (base x) (exp-before '** x))
(define (exponent x) (exp-after '** x))

; TESTS
(deriv '(x + 3 * (x + y + 2)) 'x) ; 4
(deriv '(x + 3 * (x + y + 2)) 'y) ; 3
(deriv '(x + 3 * (x + y + 2)) 'z) ; 0
(newline)

(deriv '(x + x * (2 + y)) 'x) ; 1 + 2 + y
(deriv '(x + x * (2 + y)) 'y) ; x
(deriv '(x * x * y + x + a * b * x + z) 'x) ; 2xy + 1 + ab
(newline)

(deriv '(1 + (x + -1) * (x + 1) * z) 'x) ; 2xz
(deriv '(1 + (x + -1) * (x + 1) * z) 'z) ; (x-1)*(x+1)
(newline)

(deriv '(x ** 2 * (x + 1)) 'x) ; x^2 + 2x*(x+1)
(newline)
