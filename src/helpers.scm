;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Module with helper functions

;; sum list
(define (sum ls)
  (cond ((null? ls) 0)
	(else (+ (car ls)
		 (sum (cdr ls))))))

;; product of list
(define (product ls)
  (cond ((null? ls) 1)
	(else (* (car ls)
		 (product (cdr ls))))))
