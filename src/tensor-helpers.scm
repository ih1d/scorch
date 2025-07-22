;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Helper functions for tensors

;; calculate the shape of the data
(define (calc-shape data)
  (cond ((list? data) (calc-shape-list data))
	((vector? data) (calc-shape-vector data))
	(else (error data "is not the correct type for tensor data, expected: list or vector"))))

(define (calc-shape-list data)
  (if (list? (car data))
      (cons (length data)
	    (calc-shape-list (car data)))
      (list (length data))))

(define (calc-shape-vector data)
  (if (vector? (vector-first data))
      (cons (vector-length data)
	    (calc-shape-vector (vector-first data)))
      (list (vector-length data))))

;; is the data homogenous
(define (homogenous? data)
  (cond ((list? data) (homogenous-list? data))
	((vector? data) (homogenous-vector? data))
	(else (error data "is incorrect type, expected: list or vector"))))

(define (homogenous-list? data)
  (cond ((null? data) #t)
	((every list? data)
	 (and (homogenous-list? (car data))
	      (homogenous-list? (cdr data))))
	((number? (car data))
	 (homogenous-list? (cdr data)))
	(else #f)))

(define (homogenous-vector? data)
  (cond ((= (vector-length data) 0) #t)
	((vector? (vector-first data))
	 (and (homogenous-vector? (vector-first data))
	      (homogenous-vector? (vector-tail data 1))))
	((number? (vector-first data))
	 (homogenous-vector? (vector-tail data 1)))
	(else #f)))

;; calculate the order of the tensor
(define (calc-order shape)
  (if (any (lambda (x) (= x 1)) shape)
      1
      (length shape)))

;; calculate the tensor stride
(define (calc-stride ls)
  (define (scan-go f q ls)
    (cons q (cond ((null? ls) '())
		  (else (scan-go f (f q (car ls)) (cdr ls))))))
  (scan-go * 1 ls))

;; report out of bounds
(define (report-out-of-bounds indices)
  (error indices "Index is out of bounds."))

;; out-of-bounds predicate
(define (out-of-bounds? t i)
  (let ((shape (tensor-shape t)))
    (any > i shape)))

;; predicate to determine if tensor is of rank 1
(define (is-vector? t)
  (= 1 (tensor-order t)))

;; predicate to determine if tensor is of rank 2
(define (is-matrix? t)
  (= 2 (tensor-order t)))

;; predicate to determine if tensor is a scalar
(define (is-scalar? t)
  (= 0 (tensor-order t)))
