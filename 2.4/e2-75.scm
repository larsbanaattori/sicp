#lang sicp

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define x (make-from-mag-ang 1 0))
(x 'real-part)
(x 'imag-part)
(x 'magnitude)
(x 'angle)

(define y (make-from-mag-ang (sqrt 2) (* (/ 3.14159265359 4) 3)))
(y 'real-part)
(y 'imag-part)
(y 'magnitude)
(y 'angle)
