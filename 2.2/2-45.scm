#lang sicp
(#%require sicp-pict)

(define (split op1 op2)
  (define (proc painter n)
    (if (= n 0)
        painter
        (let ((painter2 (proc painter (dec n))))
          (op1 painter (op2 painter2 painter2)))))
  proc)

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 5))
(paint (up-split einstein 5))
(paint (corner-split einstein 5))
