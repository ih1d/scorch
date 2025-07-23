;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Tensor data structure
(load "helpers.scm")
(load "tensor-helpers.scm")

;; Tensor data structure
(define-record-type :tensor
  (make-tensor data shape order dtype stride)
  tensor?
  (data tensor-data)
  (shape tensor-shape tensor-shape-set!)
  (order tensor-order)
  (dtype tensor-dtype tensor-dtype-set!)
  (stride tensor-stride tensor-stride-set!))

;; Constructor wrapper for tensors
(define (tensor data #!optional shape order dtype)
  (cond ((default-object? shape)
	 (tensor data (calc-shape data) order dtype))
	((default-object? order)
	 (tensor data shape (calc-order shape) dtype))
	((default-object? dtype)
	 (tensor data shape order 'rational))
	((homogenous? data)
	 (if (list? data)
	     (let ((stride (cdr (reverse (calc-stride (reverse shape))))))
	       (make-tensor (list->vector (flatten data)) shape order dtype stride))
	     (let ((stride (cdr (reverse (calc-stride (reverse shape))))))
	       (make-tensor (flatten-vector data) shape order dtype stride))))
	(else (error data "is not homogenous"))))

;; zeros
(define (zeros shape)
  (let ((v (make-vector (product shape) 0)))
    (tensor v shape 'rational)))

;; random tensor
(define (rand shape)
  (define (loop v i n)
    (if (= i n)
	(tensor v shape)
	(begin
	  (vector-set! v i (random 1.0))
	  (loop v (+ i 1) n))))
  (let ((n (product shape)))
    (loop (make-vector n 0) 0 n)))


;; slice a tensor
(define (tensor-ref t indices)
  (define (loop t result indices i n)
    (if (= i n)
	(tensor result)
	(let* ((stride (tensor-stride t))
	       (data (tensor-data t))
	       (k (flat-index indices (take (length indices) stride)))
	       (val (vector-ref data (+ k i))))
	  (begin
	    (vector-set! result i val)
	    (loop t result indices (+ i 1) n)))))
  (cond ((out-of-bounds? t indices)
	 (report-out-of-bounds indices))
	((and (list? indices)
	      (every integer? indices))
	 (let* ((shape (tensor-shape t))
		(n (product (drop (length indices) shape)))
		(v (make-vector n 0)))
	   (loop t v indices 0 n)))
	(else (error indices "incorrect type for tensor-ref, expected: list of integers"))))

;; transpose
(define (transpose t l)
  (cond ((or (is-vector? t)
	     (is-matrix? t))
	 (let* ((shape (tensor-shape t))
	        (rows (car shape))
	        (cols (cadr shape))
		(data (tensor-data t)))
	   '()))
	    ((null? l) '())
	    (else (error t "is too big right now!"))))
