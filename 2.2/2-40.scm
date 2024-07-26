#lang sicp

(define (filter test seq)
  (cond ((null? seq) nil)
        ((test (car seq)) (cons (car seq) (filter test (cdr seq))))
        (else (filter test (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (inc a) b))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap
   (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (dec i))))
   (enumerate-interval 1 n)))

(define (square x) (* x x))

(define (prime? n)
  (define (iter i)
    (cond ((> (square i) n) n)
          ((= (remainder n i) 0) i)
          (else (iter (inc i)))))
  (= (iter 2) n))

(define (prime-sum-pairs n)
  (define (make-pair-sum p)
    (list (car p) (cadr p) (+ (car p) (cadr p))))
  (define (is-prime-pair? p)
    (prime? (+ (car p) (cadr p))))
  (map make-pair-sum
       (filter is-prime-pair?
               (unique-pairs n))))

(prime-sum-pairs 6)
