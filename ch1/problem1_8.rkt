#lang racket
;; x/y^2 + 2y / 3 for cube guess
(define (cube-iter guess x)
  (if (improved-good-enough? guess x)
      guess
      (cube-iter (cube-improve guess x) x)))

(define (cube-improve guess x)
  (cube-average guess x))

(define (cube-average guess x)
  (/ (+ (* 2 guess) (/ x (square guess))) 3))

(define (improved-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (cube-root x)
  (cube-iter 1.0 x))

(define (abs x)
  (if (< x 0) (- x) x))
  
(cube-root 27)
; 3

(cube-root 1000)
; 10

