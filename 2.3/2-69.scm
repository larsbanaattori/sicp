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


; Make leaf set
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)     ; symbol
                               (cadr pair))   ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

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

; Generate Huffman tree
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge trees)
  (if (null? (cdr trees))
      (car trees)
      (successive-merge
       (adjoin-set
        (make-code-tree (car trees) (cadr trees))
        (cddr trees)))))

; Testing
(define tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
(define msg '(A D A B B C A))
(encode msg tree)
(decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) tree)
