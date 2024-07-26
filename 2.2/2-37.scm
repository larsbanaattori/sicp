#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector m row)) cols)))

; TESTS

(define v (list 1 2 3 4))
(define w (list 4 3 2 1))
(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

; Testing
(dot-product v w) ; 20
(dot-product v v) ; 30
(matrix-*-vector m v) ; (30, 56, 80)
(transpose m) ; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
(transpose (transpose m)) ; Same as m
(matrix-*-matrix m (transpose m)) ; ((30 56 80) (56 113 161) (80 161 230))

