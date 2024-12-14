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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Apply generic and helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-generic op . args)
  ; Internal procedures
  (define (grade arg) (apply-generic 'grade arg))
  ; The beef
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((g1 (grade a1))
                      (g2 (grade a2)))
                  (cond ((= g1 g2)
                         (error "No method for these types" (list op type-tags)))
                        ((< g1 g2)
                         (apply-generic op a1 (raise a2)))
                        (else
                         (apply-generic op (raise a1) a2)))))
              (error "No method for these types" (list op type-tags)))))))

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
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
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
  (put 'denom 'rational denom))
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
  (put 'make 'integer (lambda (x) (tag x))))
(integer-package)

(define (make-integer x) ((get 'make 'integer) x))

;;;;;;;;;;;;;;;
; Raise package
;;;;;;;;;;;;;;;

(define (raise-package)
  ; Internal definitions
  (define rat-numer (get 'numer 'rational))
  (define rat-denom (get 'denom 'rational))
  ; Interface to rest of system
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'raise '(rational) (lambda (x) (make-real (/ (rat-numer x) (rat-denom x)))))
  (put 'raise '(real) (lambda (x) (make-complex-from-real-imag x 0)))
  )

(raise-package)

(define (raise x) (apply-generic 'raise x))

;;;;;;;;;;;;;;;;;;;;;
; Type "grades"
; Smaller = higher up
;;;;;;;;;;;;;;;;;;;;;

(define (type-grade-package)
  (put 'grade '(complex) (lambda (x) 0))
  (put 'grade '(real) (lambda (x) 1))
  (put 'grade '(rational) (lambda (x) 2))
  (put 'grade '(integer) (lambda (x) 3)))
(type-grade-package)


;;;;;;;
; Tests
;;;;;;;

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define x (make-integer 4))
(define y (make-rational 7 5))
(define z (make-real 1.122345))
(define h (make-complex-from-real-imag 1 -1))

(add x y)
(sub z h)
(mul z h)
(div y h)

(add x x)
(div y y)
