#lang sicp
(#%require sicp-pict)

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
           (make-vect 1 0.25)))
    (vectors->segments
     (list (make-vect 0.40 0.85)
           (make-vect 0.50 0.70)
           (make-vect 0.60 0.85))))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (dec n))))
        (below painter
               (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (dec n))))
        (beside painter
                (below smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 
         (square-of-four identity 
                         flip-horiz
                         rotate180 
                         flip-vert)))
    (combine4 (corner-split painter n))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(paint (square-limit wave 2))
