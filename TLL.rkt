#lang racket
(require malt)

;;;; CHAPTER 1 ;;;;
(define line-first-version ;; a parametrized function
  (lambda (x) ;; arguments
    (lambda (w b) ;; parameters
      (+ (* w x) b))))

((line-first-version 8) 4 6)

;; Our first data set
(define line-xs (tensor 2.0 1.0 4.0 3.0))

(define line-ys (tensor 1.8 1.2 4.2 3.3))

;;;;; EVERY PARAMETER IS A NUMBER ;;;;;;
;; Finding the parameters of a function from a data set
;; is known as LEARNING.
(define line ;; final version
  (lambda (x)
    (lambda (little-theta)
      (+ (* (ref little-theta 0) x) (ref little-theta 1)))))

((line 7.3) (list 1.0 0.0)) ;; (Learned) How to make a y-coordinate for x-coordinates

;;;;;; ML: A function that determines little-theta for any data set ;;;;;;;

;;;; CHAPTER 2 ;;;;
(define rank
  (lambda (t)
    (ranked t 0)))

(define ranked
  (lambda (t a)
    (cond ((scalar? t) a)
          (else (ranked (tref t 0) (add1 a))))))

(define shape
  (lambda (t)
    (cond ((scalar? t) (list))
          (else (cons (tlen t)
                      (shape (tref t 0)))))))


;;;; INTERLUDE 1 ;;;;
(define sum-1
  (lambda (t)
    (summed t (sub1 (tlen t)) 0.0)))

(define summed
  (lambda (t i a)
    (cond ((zero? i) (+ (tref t 0) a))
          (else (summed t (sub1 i) (+ (tref t i) a))))))

;;;; CHAPTER 3 ;;;;
;;;; Successive Approx: finds well-fitted values ;;;;
;;;; Loss: A scalar that tells us how close or far away are we from finding a well-fitted ;;;;
(define l2-loss
  (lambda (target)
    (lambda (xs ys)
      (lambda (little-theta)
        (let ((pred-ys ((target xs) little-theta)))
          (sum
           (sqr
            (- ys pred-ys))))))))

;;;; CHAPTER 4 ;;;;