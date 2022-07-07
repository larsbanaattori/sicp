#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define z (cons 'a 'b))
(car z)
(cdr z)

; Let's check why car works through substitution rule
; (car (cons 1 2))
; (car (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) p))
; ((lambda (p q) p)) x y)
; x

; cdr works similarly
; (cdr (cons 1 2))
; (cdr (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) q))
; ((lambda (p q) q) x y)
; y
