;; Author: Isaac H. Lopez Diaz
;; Description: Algorithms for ML
(load "tensor.scm")

;; MSE
(define (mean-squared-error m y-hat y)
  (/ 1 (sigma (tensor-square (tensor- y-hat y)))
     m))
  
;; linear regression
(define (linear-regression weights x)
  (tensor* weights x))
