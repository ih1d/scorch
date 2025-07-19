;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Tensor data structure
(load "helpers.scm")

;; Tensor data structure
(define-record-type :tensor
  (make-tensor data shape order dtype)
  tensor?
  (data tensor-data)
  (shape tensor-shape)
  (order tensor-order)
  (dtype tensor-dtype))

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
	     (make-tensor (to-vector data) shape order dtype)
	     (make-tensor data shape order dtype)))
	(else (error data "is not homogenous"))))

(define (calc-shape data)
  (cond ((list? data) (calc-shape-list data))
	((vector? data) (calc-shape-vector data))
	(else (error data "is not the correct type for tensor data, expected: list or vector"))))

(define (calc-shape-list data)
  (cond ((null? data) '())
	((list? (car data))
	 (cons (length (car data))
	       (calc-shape-list (cdr data))))
	(else (list (length data)))))

(define (calc-shape-vector data)
  (cond ((vector? (vector-first data))
	 (cons (vector-length (vector-first data))
	       (calc-shape-vector (vector-tail data 1))))
	(else (list (vector-length data)))))

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
	((number? (vector-first? data))
	 (homogenous-vector? (vector-tail data 1)))
	(else #f)))

(define (calc-order shape)
  (length shape))

(define (to-vector data)
  (define (loop l v i)
    (cond ((null? l) v)
	  ((list? (car l))
	   (let ((fv (list->vector (car l))))
	     (begin
	       (vector-set! v i fv)
	       (loop (cdr l) v (+ i 1)))))
	  (else
	   (begin
	     (vector-set! v i (car l))
	     (loop (cdr l) v (+ i 1))))))
  (loop data (make-vector (length data) 0) 0))
