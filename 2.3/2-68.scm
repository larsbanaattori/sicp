#lang sicp

; Leafs
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

; Trees
(define (make-code-tree left right)
  (list
   left
   right
   (append (symbols left) (symbols right))
   (+ (weight left) (weight right))))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

; Decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOOSE-BRANCH" bit))))

; Encoding
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (eq? symbol (symbol-leaf tree))
          '()
          (error "symbol not in tree: ENCODE-SYMBOL" symbol))
      (if (memq symbol (symbols (left-branch tree)))
          (cons 0 (encode-symbol symbol (left-branch tree)))
          (cons 1 (encode-symbol symbol (right-branch tree))))))

; Testing
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define (list-eq l1 l2)
  (cond ((and (null? l1) (null? l2))
         #t)
        ((or (null? l1) (null? l2))
         #f)
        ((eq? (car l1) (car l2))
         (list-eq (cdr l1) (cdr l2)))
        (else
         #f)))

(list-eq (encode '(A D A B B C A) sample-tree) '(0 1 1 0 0 1 0 1 0 1 1 1 0)) ; #t
(encode '(A D A B B C A E) sample-tree) ; ERROR
