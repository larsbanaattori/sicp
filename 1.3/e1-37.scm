#lang sicp

(define (cont-frac n d k)
  (define (rec i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (rec (inc i))))))
  (rec 1))

(define (round-dec number n-decimals)
  (let ((scaler (expt 10 n-decimals)))
    (/ (round (* number scaler)) scaler)))

(define truth 0.61803398875)

(define truth-rounded (round-dec truth 4))

(define (phi-inverse-approx k)
  (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))

(define (find i f)
  (let ((approx (round-dec (f i) 4)))
    (let ((error (abs (- approx truth-rounded))))
      (display i)
      (display ": ")
      (display approx)
      (newline)
      (if (= error 0.0)
          (display "done")
          (find (inc i) f)))))

(find 1 phi-inverse-approx) ; 10 iterations needed
(newline)

; iterative solution. trick: start from the end
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (dec i) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (phi-inverse-approx-iter k)
  (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) k))

(find 1 phi-inverse-approx-iter)
