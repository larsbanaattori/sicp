#lang sicp

; General procedures for processing sequences
(define (filter test seq)
  (cond ((null? seq)
         nil)
        ((test (car seq))
         (cons (car seq) (filter test (cdr seq))))
        (else
         (filter test (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq)
      nil
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (map op seq)
  (if (null? seq)
      nil
      (cons (op (car seq)) (map op (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons start (enumerate-interval (inc start) end))))

; Helpers for the n-queens problem
(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? col-curr positions)
  (define (iter col-prev positions-prev)
    (if (null? positions-prev)
        true
        (let ((row-curr (car positions))
              (row-prev (car positions-prev)))
          (if (or (= row-curr row-prev)
                  (= (+ row-curr col-curr) (+ row-prev col-prev))
                  (= (- row-curr col-curr) (- row-prev col-prev)))
              false
              (iter (dec col-prev) (cdr positions-prev))))))
  (iter (dec col-curr) (cdr positions)))

; Idea
; - Queen-cols evaluates to a list of board positions (lists)
; - A board position is a list containing the rows of queens on
;   the board from k to 1 (i.e. in reverse order)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions)) ; TODO: what is safe?
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position ; TODO: what is adjoin-position?
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queens-solutions board-size)
  (length (queens board-size)))

; Tests
(queens-solutions 4) ; 2
(queens-solutions 5) ; 10
(queens-solutions 6) ; 4
(queens-solutions 7) ; 40
(queens-solutions 8) ; 92
(queens-solutions 9) ; 352
(queens-solutions 10); 724
