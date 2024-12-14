#lang sicp

;;;;;;;;;;;;;;;;;;
; Operation tables
;;;;;;;;;;;;;;;;;;

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Apply generic and helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-generic op . args)
  ; Helpers
  (define (can-coerce-to-tag? target-tag type-tags)
    (if (null? type-tags)
        #t
        (let ((coerce-op (get-coercion (car type-tags) target-tag)))
          (if (or coerce-op (eq? (car type-tags) target-tag))
              (can-coerce-to-tag? target-tag (cdr type-tags))
              #f))))
  (define (can-evaluate-at-tag? target-tag)
    (get op (map (lambda (x) target-tag) args)))
  (define (map-args-to-tag target-tag)
    (define (f arg)
      (if (eq? (type-tag arg) target-tag)
          arg
          ((get-coercion (type-tag arg) target-tag) arg)))
    (map f args))
  (define (coerce-args untried-tags type-tags)
    (if (null? untried-tags)
        #f
        (let ((target-tag (car untried-tags)))
          (if (and (can-coerce-to-tag? target-tag type-tags)
                   (can-evaluate-at-tag? target-tag))
              (map-args-to-tag target-tag)
              (coerce-args (cdr untried-tags) type-tags)))))

  ; The beef
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coerced-args (coerce-args type-tags type-tags)))
            (if coerced-args
                (apply apply-generic op coerced-args)
                (error "No method for these types: APPLY-GENERIC" (list op type-tags))))))))

; Generic procedures to handle typed datums
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (square x) (* x x))

;;;;;;;;;;;;;;;;;
; Complex numbers
;;;;;;;;;;;;;;;;;


; Polar representation
(define (complex-package-polar)
  ; Internal procedures
  (define (make-from-mag-ang r a) (cons r a))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  ; Interface to rest of system
  (define (tag z) (attach-tag 'polar z))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a)))))
(complex-package-polar)

; Rectangular presentation
(define (complex-package-rectangular)
  ; Internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ; Interface to rest of system
  (define (tag z) (attach-tag 'rectangular z))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a)))))
(complex-package-rectangular)

; Publicly facing generic selectors for complex numbers
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))

; Generic operations for complex numbers
(define (complex-package)
  ; Imported procedures from polar and rectangular packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ; Internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (real-part z1) (real-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (real-part z1) (real-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))    
  ; Interface to rest of system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a)))))
(complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;;;;;;;;;;;;;
; Real numbers
;;;;;;;;;;;;;;

(define (real-package)
  ; Interface to rest of system
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'min-3 '(real real real) (lambda (x y z) (min x y z)))
  (put 'make 'real (lambda (x) (tag (* 1.0 x)))))
(real-package)

(define (make-real x) ((get 'make 'real) x))

;;;;;;;;;;;;;;;;;;
; Rational numbers
;;;;;;;;;;;;;;;;;;

(define (rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (min3-rat x y z)
    (let ((xx (* (numer x) (denom y) (denom z)))
          (yy (* (denom x) (numer y) (denom z)))
          (zz (* (denom x) (denom y) (numer z))))
      (cond ((and (<= xx yy) (<= xx zz)) x)
            ((and (<= yy xx) (<= yy zz)) y)
            (else z))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  (put 'min-3 '(rational rational rational) (tag min3-rat)))
(rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;;;;;;;;;
; Integers
;;;;;;;;;;

(define (integer-package)
  ; Interface to rest of system
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y)
         (if (= (gcd x y) 0)
             (tag (/ x y))
             (make-rational x y))))
  (put 'min-3 '(integer integer integer) (lambda (x y z) (tag (min x y z))))
  (put 'make 'integer (lambda (x) (tag x))))
(integer-package)

(define (make-integer x) ((get 'make 'integer) x))

;;;;;;;;;;;
; Coercions
;;;;;;;;;;;

(define (coercion-package)
  ; Internal procedures
  (define rat-numer (get 'numer 'rational))
  (define rat-denom (get 'denom 'rational))
  ; Interface to rest of system
  (put-coercion 'integer 'rational (lambda (x) (make-rational (contents x) 1)))
  (put-coercion 'integer 'real (lambda (x) (make-real (contents x))))
  (put-coercion 'integer 'complex (lambda (x) (make-complex-from-real-imag (contents x) 0)))
  (put-coercion 'rational 'real (lambda (x) (make-real (/ (rat-numer (contents x)) (rat-denom (contents x))))))
  (put-coercion 'rational 'complex
                (lambda (x) (make-complex-from-real-imag (/ (rat-numer (contents x)) (contents (rat-denom x))) 0)))
  (put-coercion 'real 'complex (lambda (x) (make-complex-from-real-imag (contents x) 0))))
(coercion-package)

;;;;;;;;;;;;;;;;;;;;
; Generic operations
;;;;;;;;;;;;;;;;;;;;

(define (add x y) (apply-generic 'add x y))
(define (min-3 x y z) (apply-generic 'min-3 x y z))

;;;;;;;
; Tests
;;;;;;;

(min-3 (make-integer 1) (make-real 2.0) (make-rational 5 2)) ; 1.0
(min-3 (make-integer 1) (make-integer -1) (make-integer 0)) ; -1
(min-3 (make-real 0) (make-real 1) (make-real 10)) ; 0
(min-3 (make-complex-from-mag-ang 1 1) (make-integer 1) (make-real 2.0)) ; ERROR as it's not defined for type complex

;;;;;;;;;;;;;;;;;;;;;;
; When does what work?
;;;;;;;;;;;;;;;;;;;;;;
;
; The strategy of trying to coerce all arguments to a single type present
; amongst the arguments fails in cases doing so isn't possible, but there's
; some other type (i) to which all arguments can be coerced and (ii) which
; allows the operation to be evaluated.
;
; Take for example Figure 2.26 in the book and assume we have a square and an
; equilateral triangle that we'd like to add together to form a new polygon.
; This can be done by coercing both arguments all the way to the top of the
; type pyramid i.e. into a polygon. The approach implemented above fails to do this.
