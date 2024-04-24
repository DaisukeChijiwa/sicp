#lang racket
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)
; 5
(new-if (= 1 1) 0 5)
; 0

(define (improve guess x)
  (average guess (/ x guess)))
; make average of guess and x

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
; to see if the absolute value of the diff of x and the square of guess
; is less than 0.001

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))
; first guess is set to 1.0

(define (abs x )
  (cond ((< x 0) (- x))
        (else x)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                      x)))

(sqrt-iter 4)

; new-if doesn't have enough parameters.