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
