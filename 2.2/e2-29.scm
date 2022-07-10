#lang sicp

; First version of constructors and selectors
;(define (make-mobile left right)
;  (list left right))
;
;(define (make-branch length structure)
;  (list length structure))
;
;(define (left-branch mobile)
;  (car mobile))
;
;(define (right-branch mobile)
;  (car (cdr mobile)))
;
;(define (branch-length branch)
;  (car branch))
;
;(define (branch-structure branch)
;  (car (cdr branch)))

; Second version of constructors and selectors
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

; The two definitions that need to be rewritten when
; switching between the constructors are those for
; right-branch and branch-structure.
; The trick is that the code below manipulates
; mobiles and branches only through the procedures that
; are on the "abstraction barrier", i.e., the constructors
; and selectors.

; Common to both ways
(define (left-branch mobile)
  (car mobile))

(define (branch-length branch)
  (car branch))

(define (mobile? structure)
  (pair? structure))

; Calculates total weight of a mobile
(define (total-weight mobile)
  (define (branch-weight branch)
    (let ((structure (branch-structure branch)))
      (if (mobile? structure)
          (total-weight structure)
          structure)))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (+ (branch-weight left) (branch-weight right))))

; Some tests for total-weight
(define m1 (make-mobile
            (make-branch 10 20)
            (make-branch 20 10)))
(total-weight m1) ; 30

(define m2 (make-mobile
            (make-branch 10 m1)
            (make-branch 20 m1)))
(total-weight m2) ; 60

(define m3 (make-mobile
            (make-branch 10 8)
            (make-branch 8
                         (make-mobile
                          (make-branch 10 4)
                          (make-branch 4 16)))))
(total-weight m3) ; 28
(newline)

; Calculates if a mobile is balanced
; The idea is to calculate the balanced weight for a mobile
; by defining the balanced weight as:
; - 1 if all submobiles are not in balance OR the torques of
;   the branches are not in balance (= the mobile is not in balance)
; - The actual weight of the mobile in case it is balanced
; The added value of this added complexity (vs. just using total-weight)
; is that the tree is traversed just ones, i.e., each node is visited
; only once.
(define (balanced? mobile)
  (> (balanced-weight mobile) 0))

(define (balanced-weight structure)
  (if (not (mobile? structure))
      structure
      (let ((left (left-branch structure))
            (right (right-branch structure)))
        (let ((left-length (branch-length left))
              (right-length (branch-length right))
              (left-weight (balanced-weight (branch-structure left)))
              (right-weight (balanced-weight (branch-structure right))))
          (cond ((or (< left-weight 0) (< right-weight 0))
                 -1)
                ((> (abs (- (* right-length right-weight)
                            (* left-length left-weight)))
                   0.000001)
                 -1)
                (else
                 (+ right-weight left-weight)))))))

; Some tests
;            o
;         5  |    10
;       ----------------
;      10              5
; Weight should be 15 and it should be balanced
(define m4 (make-mobile
            (make-branch 5 10)
            (make-branch 10 5)))
(total-weight m4) ; 15
(balanced? m4) ; #t
(newline)

;             o
;         6   |    10
;       -----------------
;      10               5
; Weight should be 15 and it should not be balanced
(define m5 (make-mobile
            (make-branch 6 10)
            (make-branch 10 5)))
(total-weight m5) ; 15
(balanced? m5) ; #f
(newline)

;                       o
;                  5   |    10
;                 ----------------
;            5    | 1         3  | 2
;            -------          ------
;            1      5         2     3
; Weight should be 11 and it should not be balanced
(define m6 (make-mobile
            (make-branch 5
                         (make-mobile
                          (make-branch 5 1)
                          (make-branch 1 5)))
            (make-branch 10
                         (make-mobile
                          (make-branch 3 2)
                          (make-branch 2 3)))))
(total-weight m6) ; 11
(balanced? m6) ; #f
(newline)

;                       o
;                  5   |    10
;                 ----------------
;            5    | 1         3  | 2
;            -------          ------
;            1     5         1.2   1.8
; Weight should be 9.0 and it should be balanced
(define m7 (make-mobile
            (make-branch 5
                         (make-mobile
                          (make-branch 5 1)
                          (make-branch 1 5)))
            (make-branch 10
                         (make-mobile
                          (make-branch 3 1.2)
                          (make-branch 2 1.8)))))
(total-weight m7) ; 9.0
(balanced? m7) ; #t
(newline)

;                       o
;                  5   |    6
;                 ----------------
;            5    | 1         3  | 2
;            -------          ------
;            1      5         2     3
; Weight should be 11 and it should be balanced
(define m8 (make-mobile
            (make-branch 5
                         (make-mobile
                          (make-branch 5 1)
                          (make-branch 1 5)))
            (make-branch 6
                         (make-mobile
                          (make-branch 3 2)
                          (make-branch 2 3)))))
(total-weight m8) ; 11
(balanced? m8) ; #t
