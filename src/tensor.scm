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
  (define (loop-matrix odata ndata row col i n)
    (if (= i n)
	(tensor ndata (list col row))
	(let* ((nrow (quotient i col))
	       (ncol (remainder i col))
	       (new-i (+ (* ncol row) nrow))
	       (val (vector-ref odata i)))
	  (begin
	    (vector-set! ndata new-i val)
	    (loop-matrix odata ndata row col (+ i 1) n)))))
  (cond ((is-vector? t)
	 (let* ((rows (car (tensor-shape t)))
		(cols (cadr (tensor-shape t)))
		(data (tensor-data t)))
	   (tensor data (list cols rows))))
	((is-matrix? t)
	 (let* ((rows (car (tensor-shape t)))
		(cols (cadr (tensor-shape t)))
		(n (* rows cols))
		(data (tensor-data t))
		(new-data (make-vector n 0)))
	   (loop-matrix data new-data rows cols 0 n)))
	(else (error t "is too big for now!"))))

;; tensor add
(define (tensor+ t1 t2)
  (if (equal? (tensor-shape t1) (tensor-shape t2))
      (let* ((d1 (tensor-data t1))
	     (d2 (tensor-data t2)))
	(tensor (vector-map + d1 d2) (tensor-shape t1)))
      (error "tensors must be the same shape -- +")))

;; pretty printing a tensor
(define (print-data t port)
  (define (print-row-loop i n)
    (define (print-element-loop row j k)
      (if (= j (- k 1))
	  (display (vector-ref row j) port)
	  (begin
	    (display (vector-ref row j) port)
	    (display ", " port)
	    (print-element-loop row (+ j 1) k))))
    (if (= i (- n 1))
	(let ((row (tensor-data (tensor-ref t (list i)))))
	  (begin
	    (display "[" port)
	    (print-element-loop row 0 (vector-length row))
	    (display "]")))
	(let ((row (tensor-data (tensor-ref t (list i)))))
	  (begin
	    (display "[" port)
	    (print-element-loop row 0 (vector-length row))
	    (display "], " port)
	    (print-row-loop (+ i 1) n)))))
  (let ((rows (car (tensor-shape t))))
    (begin
      (display "[" port)
      (print-row-loop 0 rows)
      (display "]" port))))

(define (print-tensor t port)
  (display "Tensor=(" port)
  (display "data=(" port) (print-data t port) (display "), " port)
  (display "shape=(" port) (display (tensor-shape t) port) (display "), " port)
  (display "type=(" port) (display (tensor-dtype t) port) (display "))" port))

(define-print-method tensor? print-tensor)
