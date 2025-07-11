;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Tensor data structure

;; Tensor data structure
(define-record-type :tensor
  (make-tensor shape data dtype)
  tensor?
  (shape tensor-shape set-tensor-shape!)
  (data tensor-data set-tensor-data!)
  (dtype tensor-dtype set-tensor-dtype!))

;; Constructor wrapper
(define (tensor shape data . dtype)
  (make-tensor shape
	       (if (list? data) (list->vector data) data)
	       (if (null? dtype) 'f64 (car dtype))))

;; reshape function
(define (reshape t shape)
  (set-tensor-shape! t shape))


;; tensor arithmetic
(define (tensor-add t1 t2))

(define (tensor-sub t1 t2))

(define (tensor-mul t1 t2))

(define (tensor-div t1 t2))
