#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              sequence))

; Redefine count-leaves as an accumulation
(define (count-leaves t)
  (accumulate +
              0
              (map
               (lambda (t)
                 (cond ((null? t) 0)
                       ((not (pair? t)) 1)
                       (else (count-leaves t))))
               t)))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
(count-leaves (list x x))
(count-leaves (list 1 2 nil nil (list 3 nil nil)))
