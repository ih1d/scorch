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

;; flatten a list
(define (flatten l)
  (cond ((null? l) '())
	((list? (car l))
	 (append (flatten (car l))
		 (flatten (cdr l))))
	(else (cons (car l)
		    (flatten (cdr l))))))
;; flatten a vector
(define (flatten-vector v)
  (define (flatten-helper lst)
    (cond
     ((null? lst) '())
     ((vector? (car lst))
      (append (flatten-helper (vector->list (car lst)))
              (flatten-helper (cdr lst))))
     (else (cons (car lst) (flatten-helper (cdr lst))))))
  (list->vector (flatten-helper (vector->list v))))

;; take n items from a list
(define (take n l)
  (if (= n 0)
      '()
      (cons (car l)
	    (take (- n 1) (cdr l)))))

;; drop n items from a list
(define (drop n l)
  (if (= n 0)
      l
      (drop (- n 1) (cdr l))))
