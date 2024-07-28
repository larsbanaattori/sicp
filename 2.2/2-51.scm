#lang sicp
(#%require sicp-pict)

(define (transform-painter
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (vector-sub (m corner1) new-origin)
                  (vector-sub (m corner2) new-origin)))))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painter
                         painter1
                         (make-vect 0.0 0.0)
                         (make-vect 1.0 0.0)
                         split-point))
          (paint-top (transform-painter
                      painter2
                      split-point
                      (make-vect 1.0 0.5)
                      (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (apply-n-times n f arg)
  (if (= n 1)
      (f arg)
      (apply-n-times (dec n) f (f arg))))

(define (below2 painter1 painter2)
  (let ((rotated1 (rotate90 painter1))
        (rotated2 (rotate90 painter2)))
    (apply-n-times 3 rotate90 (beside rotated2 rotated1))))

(paint (below einstein mark-of-zorro))
(paint (below2 einstein mark-of-zorro))
