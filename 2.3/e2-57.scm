#lang sicp

; HELPERS
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op
                       initial
                       (cdr seq)))))

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
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation (base exp)
                                (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

; GENERIC TESTS

(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; ADDED STUFF STARTS HERE
(define (two-args? exp)
  (null? (cdddr exp)))
; ADDED STUFF ENDS HERE

; SUM
(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y))
         (+ x y))
        ; ADDED STUFF STARTS HERE
        ((and (sum? x) (sum? y))
         (append '(+) (cdr x) (cdr y)))
        ((sum? x)
         (append x (list y)))
        ((sum? y)
         (append '(+) (list x) (cdr y)))
        ; ADDED STUFF ENDS HERE
        (else (list '+ x y))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend x) (cadr x))

(define (augend x)
  ; ADDED STUFF STARTS HERE
  (if (two-args? x)
      (caddr x)
      (cons '+ (cddr x))))
  ; ADDED STUFF ENDS HERE
              
; PRODUCT
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier x) (cadr x))

(define (multiplicand x)
  (if (two-args? x)
      (caddr x)
      (cons '* (cddr x))))

(define (make-product x y)
  (cond ((or (=number? x 0)
             (=number? y 0))
         0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y))
         (* x y))
        ; ADDED STUFF STARTS HERE
        ((and (product? x) (product? y))
         (append '(*) (cdr x) (cdr y)))
        ((product? x)
         (append x (list y)))
        ((product? y)
         (append '(*) (list x) (cdr y)))
        ; ADDED STUFF ENDS HERE
        (else (list '* x y))))

; EXPONENTIATION
(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

; TESTING

; Quite a bit of tests for make-sum
;(make-sum 1 2) ; 3
;(make-sum 'a 1) ; (+ a 1)
;(make-sum 1 'a) ; (+ 1 a)
;(make-sum 'a 'b) ; (+ a b)
;(make-sum 0 'asd) ; asd
;(make-sum 'asd 0) ; asd
;(make-sum '(+ 1 2) 3) ; (+ 1 2 3)
;(make-sum 3 '(+ 1 2 3)) ; (+ 3 1 2 3)
;(make-sum '(+ 1 2 3) '(+ 4 5 6)) ; (+ 1 2 3 4 5 6)
;(make-sum '(+ 1 2 3) '(+ (* 2 2) (* a c))) ; (+ 1 2 3 (* 2 2) (* a c))
;(make-sum '(+ 1 2 3) '(* a b (** c d))) ; (+ 1 2 3 (* a b (** c d)))
;(make-sum '(+ 1 2 3) 4) ; (+ 1 2 3 4)
;(make-sum '(* a b) '(+ a s d g)) ; (+ (* a b) a s d g)
;(make-sum '4 '(+ s d)) ; (+ 4 s d)

; Some tests for make-product
;(make-product '(* a b) '(* c d)) ; (* a b c d)
;(make-product '(* a b c) '(* d e f)) ; (* a b c d e f)
;(make-product 9 9) ; 81
;(make-product 1 'thing) ; thing
;(make-product 'thing 1) ; thing
;(make-product 0 'thing) ; 0
;(make-product 'thing 0) ; 0
;(make-product 'thing '(* a b c)) ; (* thing a b c)
;(make-product '(* a b c) 'thing) ; (* a b c thing)

; Finally, some testing for deriv
(deriv '(* x y (+ x 3)) 'x) ; OK
(deriv '(* x x x) 'x) ; OK
(deriv '(+ x (* x y) (* x (+ x 1))) 'x) ; OK
