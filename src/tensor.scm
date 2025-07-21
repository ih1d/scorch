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
	       (make-tensor (flatten (to-vector data)) shape order dtype stride))
	     (let ((stride (cdr (reverse (calc-stride (reverse shape))))))
	       (make-tensor (flatten data) shape order dtype stride))))
	(else (error data "is not homogenous"))))

;; reference a value in the tensor
(define (tensor-ref t indices)
  (if (out-of-bounds? t indices)
      (report-out-of-bounds indices)
      (let* ((data (tensor-data t))
	     (stride (tensor-stride t))
	     (flat-index (apply + (map * stride indices))))
	(vector-ref data flat-index))))

;; zeros
(define (zeros shape)
  (let ((v (make-vector (product shape) 0)))
    (tensor v shape 'rational)))

;; slice row(s)
(define (slice-row t indices)
  (define (loop-for-integer t result row i n)
    (if (= i n)
	(tensor result (list 1 n))
	(let* ((stride (tensor-stride t))
	       (data (tensor-data t))
	       (flat-index (apply + (map * stride (list row i))))
	       (val (vector-ref data flat-index)))
	  (begin
	    (vector-set! result i val)
	    (loop-for-integer t result row (+ i 1) n)))))
  (cond ((out-of-bounds? t indices)
	 (report-out-of-bounds indices))
	((integer? indices)
	 (let* ((shape (tensor-shape t))
		(v (make-vector (cadr shape) 0)))
	   (loop-for-integer t v indices 0 (cadr shape))))
	((and (list? indices)
	      (every integer? indices))
	 '())
	(else (error indices "incorrect type for slicing rows, expected: integer or list of integers"))))

;; slice col(s)
(define (slice-col t indices)
  (define (loop-for-integer t result col i n)
    (if (= i n)
	(tensor result (list n 1))
	(let* ((stride (tensor-stride t))
	       (data (tensor-data t))
	       (flat-index (apply + (map * stride (list i col))))
	       (val (vector-ref data flat-index)))
	  (begin
	    (vector-set! result i val)
	    (loop-for-integer t result col (+ i 1) n)))))
  (cond ((out-of-bounds? t indices)
	 (report-out-of-bounds indices))
	((integer? indices)
	 (let* ((shape (tensor-shape t))
		(v (make-vector (car shape) 0)))
	   (loop-for-integer t v indices 0 (car shape))))	 
	((and (list? indices)
	      (every integer? indices))
	 '())
	(else (error indices "incorrect type for slicing columns, expected: integer or list of integers"))))

;; reshape
(define (reshape t shape)
  (let ((stride (cdr (reverse (calc-stride (reverse shape))))))
  (begin
    (tensor-shape-set! t shape)
    (tensor-stride-set! t stride))))

;; transpose
(define (transpose t)
  (let* ((rows (car (tensor-shape t)))
	 (cols (cadr (tensor-shape t))))
    (reshape t (list cols rows))))
