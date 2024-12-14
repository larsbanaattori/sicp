#lang sicp

; Original version of apply-generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2
                       (get-coercion type1 type2))
                      (t2->t1
                       (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))


;;;;;;;
; (1)
; Louis Reasoner's fix to a problem that maybe (?) doesn't even exist
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

; What happend if apply-generic is called with two arguments of type scheme-number
; or two arguments of type complex for an operation that is not found in the table
; for those types?
; -> Infinite loop. apply-generic keeps on coercing the types to themselves forever.
;    Nothing stops this as (i) the operation isn't defined for those types and
;    (ii) there's a coercion rule that can be used to try out (the same thing) again.

;;;;;;;
; (2)
; Is there really a problem or is Louis hallucinating?
; -> Nothing is really wrong. If the operation is defined for the relevant pair of types,
;    apply-generic will execute it since the first thing it does is to try to find the operation
;    from the table and then executes it if an operations is found.
;    If the operation doesn't exist for pair (type type), it won't be found by looking again :).
;    Hence, thworing the "No method for these types" error is the right thing to do.


;;;;;;;
; (3)
; Modify apply-generic so that it doesn't try coercion if the two arguments have the same type
; ...to protect it from Louis' ideas
(define (apply-generic op . args)
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags))))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2
                       (get-coercion type1 type2))
                      (t2->t1
                       (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))
