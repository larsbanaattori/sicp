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
            (else (error "Unknown operation sub TABLE" m))))
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
          (drop (apply proc (map contents args)))
          (if (eq-generic (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((g1 (grade a1))
                      (g2 (grade a2)))
                  (cond ((eq-generic g1 g2)
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

(define (square x) (mul x x))

;;;;;;;;;;;;;;;;;
; Complex numbers
;;;;;;;;;;;;;;;;;


; Polar representation
(define (complex-package-polar)
  ; Internal procedures
  (define (make-from-mag-ang r a) (cons r a))
  (define (make-from-real-imag x y)
    (cons (sqrt-gen (add (square x) (square y)))
          (atan-gen y x)))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z) (mul (magnitude z) (cos-gen (angle z))))
  (define (imag-part z) (mul (magnitude z) (sin-gen (angle z))))
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
    (sqrt-gen (add (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan-gen (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (mul r (cos-gen a)) (mul r (sin-gen a))))
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
     (add (real-part z1) (real-part z2))
     (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (sub (real-part z1) (real-part z2))
     (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (mul (magnitude z1) (magnitude z2))
     (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (div (magnitude z1) (magnitude z2))
     (sub (angle z1) (angle z2))))    
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
  (put 'add '(real real) (lambda (x y) (tag (add x y))))
  (put 'sub '(real real) (lambda (x y) (tag (sub x y))))
  (put 'mul '(real real) (lambda (x y) (tag (mul x y))))
  (put 'div '(real real) (lambda (x y) (tag (div x y))))
  (put 'sin '(real) (lambda (x) (tag (sin x))))
  (put 'cos '(real) (lambda (x) (tag (cos x))))
  (put 'atan '(real real) (lambda (x y) (tag (atan x y))))
  (put 'sqrt '(real) (lambda (x) (tag (sqrt x))))
  (put 'numeric '(real) (lambda (x) x))
  (put 'make 'real (lambda (x) (tag (mul 1.0 x)))))
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
      (cons (div n g) (div d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
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
  (put 'sin '(rational)
       (lambda (x) (make-real (sin (div (numer x) (denom x))))))
  (put 'cos '(rational)
       (lambda (x) (make-real (cos (div (numer x) (denom x))))))
  (put 'atan '(rational rational)
       (lambda (x y) (make-real (atan (div (numer x) (denom x)) (div (numer y) (denom y))))))
  (put 'sqrt '(rational)
       (lambda (x) (make-real (sqrt (div (numer x) (denom x))))))
  (put 'numeric '(rational)
       (lambda (x) (div (numer x) (denom x))))
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
  (put 'add '(integer integer) (lambda (x y) (tag (add x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (sub x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (mul x y))))
  (put 'div '(integer integer)
       (lambda (x y)
         (if (eq-generic (gcd x y) 0)
             (tag (div x y))
             (make-rational x y))))
  (put 'sin '(integer) (lambda (x) (make-real (sin x))))
  (put 'cos '(integer) (lambda (x) (make-real (cos x))))
  (put 'atan '(integer integer) (lambda (x y) (make-real (atan x y))))
  (put 'sqrt '(integer) (lambda (x) (make-real (sqrt x))))
  (put 'numeric '(integer) (lambda (x) x))
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
  (put 'raise 'integer (lambda (x) (make-rational (contents x) 1)))
  (put 'raise 'rational (lambda (x) (make-real (div (rat-numer (contents x)) (rat-denom (contents x))))))
  (put 'raise 'real (lambda (x) (make-complex-from-real-imag (contents x) 0)))
  )

(raise-package)

(define (raise x)
  ((get 'raise (type-tag x)) x))

;;;;;;;;;;;;;;;;;;;;;
; Type "grades"
; Smaller eq-generic higher up
;;;;;;;;;;;;;;;;;;;;;

(define (type-grade-package)
  (put 'grade '(complex) (lambda (x) 0))
  (put 'grade '(real) (lambda (x) 1))
  (put 'grade '(rational) (lambda (x) 2))
  (put 'grade '(integer) (lambda (x) 3)))
(type-grade-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Drop and path to very generic operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (project-package)
  ; Internal stuff
  (define rat-numer (get 'numer 'rational))
  ; Interface to rest of system
  (put 'project 'complex (lambda (x) (make-real (numeric (real-part (contents x))))))
  (put 'project 'real (lambda (x) (make-rational (round (contents x)) 1)))
  (put 'project 'rational (lambda (x) (make-integer (rat-numer (contents x))))))
(project-package)

(define (project x)
  ((get 'project (type-tag x)) x))

(define (eq-package)
  (define rat-numer (get 'numer 'rational))
  (define rat-denom (get 'denom 'rational))
  (put 'eq '(complex complex)
       (lambda (x y)
         (or (and (eq-generic (real-part x) (real-part y)) (eq-generic (imag-part x) (imag-part y)))
             (and (eq-generic (magnitude x) (magnitude y)) (eq-generic (angle x) (angle y))))))
  (put 'eq '(real real) =)
  (put 'eq '(rational rational)
       (lambda (x y) (and (eq-generic (rat-numer x) (rat-numer y)) (eq-generic (rat-denom x) (rat-denom y)))))
  (put 'eq '(integer integer) =))
(eq-package)

(define (convert-to-type number-like)
    (if (number? number-like)
        (drop (make-real number-like))
        number-like))

(define (make-generic-op number-proc type-proc-name num-args)
  (cond ((= 2 num-args)
         (lambda (x y)
           (if (and (number? x) (number? y))
               (number-proc x y)
               (apply-generic type-proc-name (convert-to-type x) (convert-to-type y)))))
        ((= 1 num-args)
         (lambda (x)
           (if (number? x)
               (number-proc x)
               (apply-generic type-proc-name (convert-to-type x)))))
        (else
         (error "MAKE-GENERIC-OP not defined for num-args =" num-args))))

(define eq-generic (make-generic-op = 'eq 2))

(define (drop x)
  (if (pair? x)
      (let ((project-proc (get 'project (type-tag x))))
        (if (and project-proc (eq-generic x (raise (project x))))
            (drop (project x))
            x))
      x))

(define add (make-generic-op + 'add 2))
(define sub (make-generic-op - 'sub 2))
(define mul (make-generic-op * 'mul 2))
(define div (make-generic-op / 'div 2))
(define sin-gen (make-generic-op sin 'sin 1))
(define cos-gen (make-generic-op cos 'cos 1))
(define atan-gen (make-generic-op atan 'atan 2))
(define sqrt-gen (make-generic-op sqrt 'sqrt 1))
(define numeric (make-generic-op (lambda (x) x) 'numeric 1))

;;;;;;;
; Tests
;;;;;;;

; Does previous stuff work
(newline)
(eq-generic
 (add (make-integer 3) (make-integer 4))
 (make-integer 7))
(eq-generic
 (div (make-integer 8) (make-integer 4))
 (make-integer 2))
(eq-generic
 (mul (make-complex-from-real-imag 0 0.5) (make-complex-from-real-imag 0 -4))
 (make-integer 2))
(eq-generic
 (div (make-rational 3 2) (make-rational 3 2))
 (make-integer 1))
(eq-generic
 (sub (make-complex-from-real-imag 1 1)
      (make-rational 1 2))
 (make-complex-from-real-imag 0.5 1))
(newline)

; Does new stuff work?
(eq-generic
 (add
  (make-integer 10)
  (div (make-complex-from-mag-ang (make-integer 5) (make-rational 12 6))
       (make-complex-from-mag-ang 2 (make-real 2))))
 (make-real 12.5))

; Here's what was done to enable using whatever number types, either built-in or not,
; to construct complex numbers:
; - The arithmetic procedures that are used to define operations on complex numbers were replaced by
;   generic operations that run on both built-in numbers and the types we've created
;   - These procedures are (original -> new generic one):
;     + -> add
;     - -> sub
;     * -> mul
;     / -> div
;    sin -> sin-gen
;    cos -> cos-gen
;    atan -> atan-gen
;    sqrt -> sqrt-gen
;   - I was actually lazy and did the same changes to other number packages using find-replace.
;     This is fine since the genetic procedures are defined so that they work on built-in numbers too.
;     The generic procedures now first dispatch on whether the number is a built-in one or not.
;     Then, it uses apply-generic to dispatch on type if built-in numbers are not being applied.
;     In case one of the arguments is a built-in number and the other is not, the built-in one
;     is coerced to the real number type we've defined earlier
;   - We could've also modified attach-tag, type-tag and contents so that the rest of the program thinks that
;     built-in numbers have a type-tag. Then, we would've been able to use the pre-existing logic for
;     coercion and dispatching based on type, but we would've had to create a separate package for
;     built-in numbers. Both ways to go have their pros and cons.
; - Projecting complex numbers to real ones is modified so that non built-in arguments for the real-part
;   are coerced to built-in numbers. This is done to prevent silly cases where e.g. real numbers are
;   defined by a combination of a type tag and a non-built-in real number. In other words,
;   we allow non-built-in numbers to be used within a non-built-in representation only in case
;   of complex numbers.
