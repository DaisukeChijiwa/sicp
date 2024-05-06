#lang racket
(define (fast-exp-iter b n)
  (exp-iter b n 1))

(define (exp-iter b n a)
  (cond ((= n 0) a)
    ((even? n)(exp-iter(* b b)(/ n 2) a))
    (else (exp-iter b (- n 1) (* b a)))))

(define (even? n)
  (= (remainder n 2) 0))

(fast-exp-iter 7 9)