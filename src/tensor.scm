;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Tensor data structure
(load "helpers.scm")

;; Tensor data structure
(define-record-type :tensor
  (make-tensor shape data dtype)
  is-tensor?
  (shape tensor-shape set-tensor-shape!)
  (data tensor-data set-tensor-data!)
  (dtype tensor-dtype set-tensor-dtype!))

;; Constructor wrapper
(define (tensor shape data . dtype)
  (cons
   (make-tensor shape
	       (if (list? data) (list->vector data) data)
	       (if (null? dtype) 'f64 (car dtype)))
   (cdr (reverse (make-stride (reverse shape))))))

;; wrapper for tensor predicate
(define (tensor? a)
  (is-tensor? (car a)))

;; make stride helper
(define (make-stride ls)
  (define (scan-go f q ls)
    (cons q (cond ((null? ls) '())
		  (else (scan-go f (f q (car ls)) (cdr ls))))))
  (scan-go * 1 ls))

;; zeros
(define (zeros shape . dtype)
  (if (list? shape)
      (let* ((size (product shape)))
	(tensor shape (make-vector size 0) dtype))
      (tensor (list shape) (make-vector size 0) dtype)))

;; rand
(define (rand shape . dtype)
  (define (generate-random number)
    (cond ((= number 0) '())
	  (else (cons (random 1.0)
		      (generate-random (- number 1))))))
  (if (list? shape)
      (let* ((size (product shape))
	     (random-numbers (generate-random size)))
	(tensor shape (list->vector random-numbers) dtype))
      (let ((random-numbers (generate-random shape)))
	(tensor (list shape) (list->vector random-numbers) dtype))))

;; access tensor row
(define (tensor-row t data idx i n)
  (if (= i n)
      (tensor (list n) data (tensor-dtype (car t)))
      (begin
	(vector-set! data i (tensor-ref t (list idx i)))
	(tensor-row t data idx (+ i 1) n))))

;; access tensor element
(define (tensor-ref t indices)
  (let* ((shape (tensor-shape (car t)))
	 (data (tensor-data (car t)))
	 (strides (cdr t)))
    (cond ((out-of-bounds? indices shape)
	   (error indices "Are out of bounds!"))
	  ((< (length indices) (length shape))
	   (let ((v (make-vector (cadr shape))))
	     (tensor-row t v (car indices) 0 (cadr shape))))
	  (else
	   (let ((flat-index (apply + (map * strides indices))))
	     (vector-ref data flat-index))))))

(define (tensor-set! t indices val)
  (let* ((shape (tensor-shape (car t)))
	 (data (tensor-data (car t)))
	 (strides (cdr t))
	 (flat-index (apply + (map * strides indices))))
    (vector-set! data flat-index val)))

;; check if indices are out of bounds
(define (out-of-bounds? idx shape)
  (or (> (length idx) (length shape))
      (any > idx shape)))

;; reshape function
(define (reshape t shape)
  (set-tensor-shape! (car t) shape))

;; checks if tensors have same shape
(define (same-shape? t1 t2)
  (equal? (tensor-shape (car t1)) (tensor-shape (car t2))))

;; check if vector
(define (is-vector? t)
  (null? (cdr (tensor-shape (car t)))))

;; same rows
(define (same-rows? t1 t2)
  (equal? (car (tensor-shape (car t1)))
	  (car (tensor-shape (car t2)))))

;; same columns
(define (same-columns? t1 t2)
  (cond ((and (is-vector? t1)
	      (is-vector? t2))
	 (equal? (car (tensor-shape (car t1)))
		 (car (tensor-shape (car t2)))))
	((is-vector? t1)
	 (equal? (car (tensor-shape (car t1)))
		 (cadr (tensor-shape (car t2)))))
	((is-vector? t2)
	 (equal? (cadr (tensor-shape (car t1)))
		 (car (tensor-shape (car t2)))))
	(else
	 (equal? (cadr (tensor-shape (car t1)))
		 (cadr (tensor-shape (car t2)))))))

;; transpose
(define (transpose t)
  (define (outer-loop A m n i j res)
    (define (inner-loop A m n i j res)
      (if (not (= j n))
	  (begin
	    (tensor-set! res (list j i) (tensor-ref A (list i j)))
	    (inner-loop A m n i (+ j 1) res)))	  )
    (if (not (= i m))
	(begin
	  (inner-loop A m n i j res)
	  (outer-loop A m n (+ i 1) 0 res))
	res))
  (let* ((shape (tensor-shape (car t)))
	 (rows (car shape))
	 (cols (cadr shape))
	 (c (tensor (list cols rows)
		    (make-vector (* rows cols) 0)
		    (tensor-dtype (car t)))))
    (outer-loop t rows cols 0 0 c)))

;; dot product
(define (dot t1 t2)
  (define (dot-product v1 v2 i n)
    (if (= i n)
	0
	(+ (dot-product v1 v2 (+ i 1) n)
	   (* (vector-ref v1 i)
	      (vector-ref v2 i)))))
  (if (same-shape? t1 t2)
      (dot-product (tensor-data (car t1)) (tensor-data (car t2)) 0 (vector-length (tensor-data (car t1))))
      (error "Tensors are not of the same dimension -- DOT")))

;; saxpy
(define (saxpy t1 t2 a)
  (define (saxpy-helper v1 v2 i n)
    (if (not (= i n))
	(begin
	  (vector-set! v2 i (+ (vector-ref v2 i)
			       (* (vector-ref v1 i) a)))
	  (saxpy-helper v1 v2 (+ i 1) n))))
  (if (same-shape? t1 t2)
      (saxpy-helper (tensor-data (car t1)) (tensor-data (car t2)) 0 (vector-length (tensor-data (car t1))))
      (error "Tensors are not of the same dimension -- SAXPY.")))

;; gaxpy (row-oriented)
(define (gaxpy mat v1 v2)
  (define (gaxpy-helper A x y i j m n)
    (define (inner-loop A x i j n)
      (if (not (= j n))
	  (let* ((aij (tensor-ref A (list i j)))
		 (xj (tensor-ref x (list j))))
	    (+ (inner-loop A x i (+ j 1) n)
	       (* aij xj)))
	  0))
    (if (not (= i m))
	(let* ((yi (+ (tensor-ref y (list i))
		      (inner-loop A x i j n))))
	  (begin
	    (vector-set! (tensor-data (car y)) i yi)
	    (gaxpy-helper A x y (+ i 1) 0 m n)))))
  (if (and (same-rows? mat v2)
	   (same-columns? mat v1))
      (gaxpy-helper mat v1 v2 0 0
		    (vector-length (tensor-data (car v2)))
		    (vector-length (tensor-data (car v1))))
      (error "Tensors are not of the same dimension -- GAXPY.")))

;; addition
(define (tensor+ a b)
  (cond ((and (number? a)
	      (number? b)) (+ a b))
	((and (tensor? a)
	      (number? b)) (tensor+scalar a b))
	((and (number? a)
	      (tensor? b)) (tensor+scalar b a))
	((and (tensor? a)
	      (tensor? b)) (tensor+tensor a b))
	(else (error a b "Are incorrect types -- +"))))

(define (tensor+scalar a b)
  (let* ((data (tensor-data (car a)))
	 (shape (tensor-shape (car a)))
	 (dtype (tensor-dtype (car a))))
    (tensor shape (vector-map (lambda (x) (+ x b)) data) dtype)))

(define (tensor+tensor a b)
  (define (outer-loop A B C i j m n)
    (define (inner-loop A B C i j m n)
      (if (not (= j n))
	  (let* ((aij (tensor-ref A (list i j)))
		 (bij (tensor-ref B (list i j))))
	    (begin
	      (tensor-set! C (list i j) (+ aij bij))
	      (inner-loop A B C i (+ j 1) m n)))))	  	      
    (if (not (= i m))
	(begin
	  (inner-loop A B C i j m n)
	  (outer-loop A B C (+ i 1) 0 m n))
	C))
  (cond ((and (is-vector? a)
	      (is-vector? b))
	 (let* ((adata (tensor-data (car a)))
		(bdata (tensor-data (car b))))
	   (tensor (tensor-shape (car a))
		   (vector-map + adata bdata)
		   (tensor-dtype (car a)))))
	((same-shape? a b)
	 (let* ((shape (tensor-shape (car a)))
		(m (car shape))
		(n (cadr shape))
		(c (tensor shape (make-vector (* m n) 0) (tensor-dtype (car a)))))
	   (outer-loop a b c 0 0 m n)))
	(else (error "Tensors are not of the same shape -- +."))))

;; difference
(define (tensor- a b)
  (cond ((and (number? a)
	      (number? b)) (- a b))
	((and (tensor? a)
	      (number? b)) (tensor-scalar a b))
	((and (number? a)
	      (tensor? b)) (tensor-scalar b a))
	((and (tensor? a)
	      (tensor? b)) (tensor-tensor a b))
	(else (error a b "Are incorrect types -- -"))))

(define (tensor-scalar a b)
  (let* ((data (tensor-data (car a)))
	 (shape (tensor-shape (car a)))
	 (dtype (tensor-dtype (car a))))
    (tensor shape (vector-map (lambda (x) (- x b)) data) dtype)))

(define (tensor-tensor a b)
  (define (outer-loop A B C i j m n)
    (define (inner-loop A B C i j m n)
      (if (not (= j n))
	  (let* ((aij (tensor-ref A (list i j)))
		 (bij (tensor-ref B (list i j))))
	    (begin
	      (tensor-set! C (list i j) (- aij bij))
	      (inner-loop A B C i (+ j 1) m n)))))	  	      
    (if (not (= i m))
	(begin
	  (inner-loop A B C i j m n)
	  (outer-loop A B C (+ i 1) 0 m n))
	C))
  (cond ((and (is-vector? a)
	      (is-vector? b))
	 (let* ((adata (tensor-data (car a)))
		(bdata (tensor-data (car b))))
	   (tensor (tensor-shape (car a))
		   (vector-map - adata bdata)
		   (tensor-dtype (car a)))))
	((same-shape? a b)
	 (let* ((shape (tensor-shape (car a)))
		(m (car shape))
		(n (cadr shape))
		(c (tensor shape (make-vector (* m n) 0) (tensor-dtype (car a)))))
	   (outer-loop a b c 0 0 m n)))
	(else (error "Tensors are not of the same shape -- +."))))

;; multiplication
(define (tensor* a b)
  (cond ((and (number? a)
	      (number? b)) (* a b))
	((and (tensor? a)
	      (number? b)) (tensor*scalar a b))
	((and (number? a)
	      (tensor? b)) (tensor*scalar b a))
	((and (tensor? a)
	      (tensor? b)) (tensor*tensor a b))
	(else (error a b "Are incorrect types -- *"))))

(define (tensor*scalar a b)
  (let* ((data (tensor-data (car a)))
	 (shape (tensor-shape (car a)))
	 (dtype (tensor-dtype (car a)))
	 (new-data (vector-map (lambda (x) (* x b)) data)))
    (tensor shape new-data dtype)))

(define (vector*vector a b)
  (define (loop a b c i n)
    (if (= i n)
	c
	(begin
	  (tensor-set! c (list i)
		     (* (vector-ref a i)
			(vector-ref a i)))
	  (loop a b c (+ i 1) n))))
  (if (= (vector-length (tensor-data (car a)))
	 (vector-length (tensor-data (car b))))
      (loop (tensor-data (car a))
	    (tensor-data (car b))
	    (tensor (tensor-shape (car a))
		    (make-vector (vector-length (tensor-data (car a))))
		    (tensor-dtype (car a)))
	    0
	    (vector-length (tensor-data (car a))))
      (error a b "Have different lengths -- *")))

(define (tensor*tensor a b)
  (define (outer-loop A B C i j k m n r)
    (define (inner-loop A B C i j k m n r)
      (define (inner-inner-loop A B C i j k m n r)
	(if (not (= k r))
	    (let* ((aik (tensor-ref A (list i k)))
		   (bkj (tensor-ref B (list k j)))
		   (cij (tensor-ref C (list i j))))
	      (begin
		(tensor-set! C (list i j) (+ cij (* aik bkj)))
		(inner-inner-loop A B C i j (+ k 1) m n r)))))
      (if (not (= j n))
	  (begin
	    (inner-inner-loop A B C i j k m n r)
	    (inner-loop A B C i (+ j 1) 0 m n r))))
    (if (not (= i m))
	(begin
	  (inner-loop A B C i j k m n r)
	  (outer-loop A B C (+ i 1) 0 0 m n r))
	C))
  (cond ((and (is-vector? a)
	      (is-vector? b))
	 (vector*vector a b))
	((is-vector? a) (gaxpy b a))
	((is-vector? a) (gaxpy a b))
	(else
	 (let* ((a-shape (tensor-shape (car a)))
		(b-shape (tensor-shape (car b)))
		(m (car a-shape))
		(n (cadr b-shape))
		(r1 (cadr a-shape))
		(r2 (car b-shape)))
	   (if (= r1 r2)
	       (let ((c (tensor (list m n) (make-vector (* m n) 0))))
		 (outer-loop a b c 0 0 0 m n r1))
	       (error "Tensors have incompatible dimensions -- MATMUL."))))))

;; outer product
(define (outer-product a x y)
  (define (outer-product-helper a x y i j m n)
    (define (inner-loop a x y i j n)
      (if (not (= j n))
	  (let* ((aij (tensor-ref a (list i j)))
		 (xi (tensor-ref x (list i)))
		 (yj (tensor-ref y (list j))))
	    (begin
	      (tensor-set! a (list i j) (+ aij (* xi yj)))
	      (inner-loop a x y i (+ j 1) n)))))
    (if (not (= i m))
	(begin
	  (inner-loop a x y i j n)
	  (outer-product-helper a x y (+ i 1) 0 m n))))
  (if (and (same-rows? a x)
	   (same-columns? a y))
      (outer-product-helper a x y 0 0
			    (vector-length (tensor-data (car x)))
			    (vector-length (tensor-data (car y))))
      (error "Tensors are not of the same dimension -- OUTER-PRODUCT.")))

;; sum
(define (sigma v)
  (define (loop v i n)
    (if (= i n)
	0
	(+ (vector-ref v i)
	   (loop v (+ i 1) n))))
  (if (is-vector? v)
      (loop (tensor-data (car v))
	    0
	    (vector-length (tensor-data (car v))))
      (error v "Should be a vector -- SIGMA")))

;; square a tensor
(define (tensor-square t)
  (let* ((shape (tensor-shape (car t)))
	 (data (tensor-data (car t)))
	 (dtype (tensor-dtype (car t))))
    (tensor shape (vector-map (lambda (x) (* x x)) data) dtype)))
