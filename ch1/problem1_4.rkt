#lang racket
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; if b is positive integer, return a+b else return a-b
(a-plus-abs-b 5 2)
(a-plus-abs-b 5 -2)