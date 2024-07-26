#lang sicp

; Constructors and selectors
(define (make-mobile left right)
  (cons left right))
(define (left-branch m) (car m))
(define (right-branch m) (cdr m))

(define (make-branch length structure)
  (cons length structure))
(define (branch-length b) (car b))
(define (branch-structure b) (cdr b))

; Calculates total weight of the mobile
(define (total-weight mobile)
  (if (pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

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

; Calculates if the mobile is balanced
(define (branch-torque branch)
  (* (total-weight (branch-structure branch)) (branch-length branch)))

(define (balanced? mobile)
  (define (close-enough? a b)
    (< (abs (- a b)) 1e-9))
  (if (pair? mobile)
      (let ((left (left-branch mobile))
            (right (right-branch mobile)))
        (and
         (close-enough? (branch-torque left) (branch-torque right))
         (balanced? (branch-structure left))
         (balanced? (branch-structure right))))
      #t))

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

; If changing the "glue" in make-mobile and make-branch from list to cons,
; the only things we need to change are constructors for right-branch and
; branch-structure. Those need to be changed from cadr to cdr.
; Everything else works since we're using the selectors, not the detailed
; implementation, of mobiles and branches in all procedures that utilize them.
