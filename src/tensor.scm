;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Tensor data structure
(load "helpers.scm")
(load "tensor-helpers.scm")

;; Tensor data structure
(define-record-type :tensor
  (make-tensor data shape order dtype stride)
  tensor?
  (data tensor-data)
  (shape tensor-shape)
  (order tensor-order)
  (dtype tensor-dtype)
  (stride tensor-stride))

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
  (let* ((data (tensor-data t))
	 (stride (tensor-stride t))
	 (flat-index (apply + (map * stride indices))))
    (vector-ref data flat-index)))
