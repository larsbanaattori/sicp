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

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (square x) (* x x))

(define (prime? n)
  (define (iter i)
    (cond ((> (square i) n)
           n)
          ((= (remainder n i) 0)
           i)
          (else
           (iter (inc i)))))
  (= (iter 2) n))

(define (filter test seq)
  (cond ((null? seq) nil)
        ((test (car seq))
         (cons (car seq) (filter test (cdr seq))))
        (else (filter test (cdr seq)))))

(define (prime-sum-pairs n)
  (define (make-pair-sum pair)
    (list (car pair)
          (cadr pair)
          (+ (car pair) (cadr pair))))
  (define (prime-pair? pair)
    (prime? (+ (car pair) (cadr pair))))
  (map
   make-pair-sum
   (filter
    prime-pair?
    (unique-pairs n))))

(prime-sum-pairs 6)
