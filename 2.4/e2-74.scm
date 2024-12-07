#lang sicp

; (1)
; Individual divisions' files should contain both:
; - a tag naming the division, which can be retrieved with proc type-tag
; - contents, which can be retrieved with proc contents
; The type-procedure lookup table that proc get operates on
; has to contain a specific get-record function for each division's
; file format.
(define (get-record emp file)
  ((get 'get-record (type-tag file)) emp (contents file)))

; (2)
; Here were are not placing restrictions on how each division's
; file is represented. Instead, we merely require that the lookup
; table that proc get operates on contains a functioning get-salary proc
; for each division
(define (get-salary emp file)
  ((get 'get-salary (type-tag file))
   (get-record emp file)))

; (3)
; Assume that files is a list containing the files of all divisions.
(define (find-employee-record emp files)
  (if (null? files)
      (error "EMPLOYEE RECORD NOT FOUND:" emp)
      (let ((record (get-record emp (car files))))
        (if (null? record)
            (find-employee-record emp (cdr files))
            record))))

; (4)
; The only change needed is to add properly working procedures
; get-record and get-salary to the look-up table containing
; the right version of each procedure for each division.
; Also, if not clear, the new division's file needs to be
; complemented with a type tag
; All of these changes are additive thanks to usage of data-directed
; programming.
