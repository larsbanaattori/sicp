#lang sicp

; HELPERS

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

; QUEENS

(define (queens board-size)
  ; Helpers
  (define empty-board nil)
  (define (adjoin-position new-row k rest-of-queens)
    (cons (cons new-row k) rest-of-queens))
  (define (safe? k positions)
    (define (conflict? pos1 pos2)
      (let ((r1 (car pos1))
            (r2 (car pos2))
            (c1 (cdr pos1))
            (c2 (cdr pos2)))
        (or
         (= r1 r2)
         (= (+ r1 c1) (+ r2 c2))
         (= (- r1 c1) (- r2 c2)))))
    (define (conflicts? curr_pos positions)
      (if (null? positions)
          #f
          (or (conflict? curr_pos (car positions))
              (conflicts? curr_pos (cdr positions)))))
    (not (conflicts? (car positions) (cdr positions))))

  ; The big show
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
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
