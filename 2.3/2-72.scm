#lang sicp

; Encoding from Ex 2.72
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

; Assume that relative frequencies of symbols are
; powers of 2 i.e. 1,2,4,...,2^(n-1) like we did in Ex 2.71.
; 
; Case of most frequent symbol: O(1)
;
; We only need to figure out that the symbol matches
; that in the first node to the left of the root node.
; This takes constant time since the symbol list of that
; node contains only the most frequent symbol i.e.
; it's length doesn't depend on n

; Case of least frequent symbol: O(n)
;
; In each node of the tree, we'll compare the symbol
; to the one in the left branch. The left branch is always
; a leaf i.e. contains only one symbol thanks to the
; assumption we made about relative frequencies. This means
; that we have a constant amount of computation to be done
; in each node. Note that we use cons to glue the answer
; together, which also runs in constant time.
; There's O(n) of nodes we need to visit to reach the
; right-most node of the tree. Thus, the total time
; for least frequent symbol is O(n)
