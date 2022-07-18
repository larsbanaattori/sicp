#lang sicp
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
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation (base exp)
                                (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (addend x) (cadr x))

(define (augend x) (caddr x))

(define (multiplier x) (cadr x))

(define (multiplicand x) (caddr x))

(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y))
         (+ x y))
        (else (list '+ x y))))

(define (make-product x y)
  (cond ((or (=number? x 0)
             (=number? y 0))
         0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y))
         (* x y))
        (else (list '* x y))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(deriv '(+ x 3) 'x) ; 1
(deriv '(* x x) 'x) ; 2x
(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; y
(deriv '(* (* x y) (+ x 3)) 'x) ; 3y + 2xy
(deriv '(+ (** x 3)
           (* a (** x 2)))
       'x) ; 3x^2 + 2ax
(deriv '(** (* 2 x) 4) 'x) ; 4 * (2x)^3 * 2
(deriv '(** (* 2 x) 4) 'y) ; 0
