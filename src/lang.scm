;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Language Expressions
(load "tensor.scm")

;; Predicates
(define (constant? e)
  (or (number? e)
      (boolean? e)
      (tensor? e)))

