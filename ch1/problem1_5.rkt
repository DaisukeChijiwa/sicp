#lang racket
(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

(test 0 (p))
; test returns 0 if x is 0 else returns y.
; In normal-order evaluation,
; (test 0 (p)) returns (p) because x is 0.
; (p) returns (p) and it is an endless loop.
; In applicative-order evaluation,
; since the evaluation of (p) happens before the evaluation of test,
; which invokes an endless loop inside test parameter, 
; test will never be evaluated.