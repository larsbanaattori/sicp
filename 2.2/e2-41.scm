#lang sicp
(define (enumerate-interval i n)
  (if (> i n)
      nil
      (cons i (enumerate-interval (inc i) n))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (filter test seq)
  (cond ((null? seq) nil)
        ((test (car seq))
         (cons (car seq) (filter test (cdr seq))))
        (else (filter test (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (tail)
                    (cons i tail))
                  (unique-pairs (dec i))))
           (enumerate-interval 1 n)))

(define (s-sum-triples n s)
  (filter
   (lambda (triple) (= (accumulate + 0 triple) s))
   (unique-triples n)))

(unique-triples 5)
(s-sum-triples 5 9) ; ((4 3 2) (5 3 1))
(s-sum-triples 6 10) ; ((5 4 1) (5 3 2) (6 3 1))
