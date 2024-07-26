#lang sicp

(define (accumulate op nil seq)
  (if (null? seq)
      nil
      (op (car seq)
          (accumulate op
                      nil
                      (cdr seq)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; Tests
(define (square x) (* x x))
(map square (list 1 2 3 4 5))
(append (list 1 2 3) (list 4 5 6))
(length (list 1 2 3 4 5 6))
