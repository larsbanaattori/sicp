#lang sicp
(#%require sicp-pict)

(define c1 (make-vect 0 0))
(define c2 (make-vect 0 1))
(define c3 (make-vect 1 1))
(define c4 (make-vect 1 0))
(define cc1 (make-vect 0 0.5))
(define cc2 (make-vect 0.5 1))
(define cc3 (make-vect 1 0.5))
(define cc4 (make-vect 0.5 0))

(define outline
  (segments->painter
   (list (make-segment c1 c2)
         (make-segment c2 c3)
         (make-segment c3 c4)
         (make-segment c4 c1))))

(define cross
  (segments->painter
   (list (make-segment c1 c3)
         (make-segment c2 c4))))

(define diamond
  (segments->painter
   (list (make-segment cc1 cc2)
         (make-segment cc2 cc3)
         (make-segment cc3 cc4)
         (make-segment cc4 cc1))))

(define (vectors->segments vectors)
  (if (null? (cdr vectors))
      nil
      (cons (make-segment
             (car vectors)
             (cadr vectors))
            (vectors->segments (cdr vectors)))))

(define wave
  (segments->painter
   (append
    (vectors->segments
     (list (make-vect 0.25 0)
           (make-vect 0.3 0.5)
           (make-vect 0.25 0.6)
           (make-vect 0.17 0.45)
           (make-vect 0 0.65)))
    (vectors->segments
     (list (make-vect 0 0.85)
           (make-vect 0.17 0.62)
           (make-vect 0.25 0.7)
           (make-vect 0.35 0.7)
           (make-vect 0.3 0.85)
           (make-vect 0.35 1)))
    (vectors->segments
     (list (make-vect 0.35 0)
           (make-vect 0.5 0.3)
           (make-vect 0.65 0)))
    (vectors->segments
     (list (make-vect 0.65 1)
           (make-vect 0.7 0.85)
           (make-vect 0.65 0.7)
           (make-vect 0.8 0.7)
           (make-vect 1 0.4)))
    (vectors->segments
     (list (make-vect 0.75 0)
           (make-vect 0.65 0.5)
           (make-vect 1 0.25))))))

; Tests
(paint outline)
(paint cross)
(paint diamond)
(paint wave)
