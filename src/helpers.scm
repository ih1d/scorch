;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Helper functions on lists & vectors

;; sum of a list
(define (sum l)
  (cond ((null? l) 0)
	(else (+ (car l)
		 (sum (cdr l))))))

;; product of a list
(define (product l)
  (cond ((null? l) 1)
	(else (* (car l)
		 (product (cdr l))))))

;; sum of a vector
(define (vsum v)
  (define (loop v i n)
    (if (= i n)
	0
	(+ (vector-ref v i)
	   (loop v (+ i 1) n))))
  (loop v 0 (vector-length v)))

;; product of a vector
(define (vproduct v)
  (define (loop v i n)
    (if (= i n)
	1
	(* (vector-ref v i)
	   (loop v (+ i 1) n))))
  (loop v 0 (vector-length v)))

;; list (of list) to vector (of vector)
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

;; flatten a vector
(define (flatten v)
  (cond	((and (vector? (vector-first v))
	      (= (vector-length v) 1))
	 (vector-first v))
	((vector? (vector-first v))
	 (vector-append (flatten (vector-first v))
			(flatten (vector-tail v 1))))
	(else v)))
