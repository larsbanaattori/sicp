#lang sicp

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; Partial-tree works as follows:
; 0. If n = 0, it returns a pair of an empty tree and elts. Otherwise it
; 1. Determines how many elements the left sub-tree should contain (quotient of (n-1) and 2)
;    - Note that the n elements are split evenly to left and right subtrees since the result
;      should be a perfectly balanced tree
; 2. Uses recursion to evaluate the left sub-tree via (partial-tree elts left-size)
;    - The recursion works since it will eventually run into an empty tree
; 3. Determines how many elements the right sub-tree should contain (n - 1 - size of left sub-tree)
; 4. Uses recursion to evaluate the right sub-tree via (partial-tree (cdr non-left-elts right-size)
; 5. Glues the resulting tree together via (make-tree this-entry left-tree right-tree)
; 6. Returns a pair consisting of the tree and remaining elements
;
; The magic is in recursion. The elements to be transformed into a perfectly balanced tree
; are split into three components
; - The elements to be transformed into the left sub-tree
; - The root value
; - The elements to be transformed into the left sub-tree
; Then, the procedure is called recursively to evaluate the sub-trees.
; The tree is perfectly balanced as the elements to be transformed into the sub-trees
; are split evenly between branches. Order is preserved since this split occurs in the
; original order of the (ordered) input list

(list->tree '(1 3 5 7 9 11))
;           5
;      1         9
;    _    3   7     11

; The order of growth is O(n).
; The running time can be expressed by recurrence relation
; T(n) = 2 * T(n/2) + O(1) as the procedure
; - Splits the input into two sets ands solves those cases recursively, and
; - Does "overhead" operations (arithmetics, cars, cdrs, and cons) which run in
;   constant time with respect to input size.
;
; T(n) = n is a solution to the recurrence relation
;
; The Master Theorem could be applied for a more rigorous proof.
;
; Intuitively, the procedure traverses the list and glues elements
; to the appropriate data structure (binary tree). The key is that the
; "gluing" takes constant time in each node of the recursion tree, and that
; the number of nodes visited by the tree is O(n).
