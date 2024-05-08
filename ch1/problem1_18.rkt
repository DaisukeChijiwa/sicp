#lang racket
(define (mul a b)
  (mul-iter a b 0))

(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (mul-iter a b n)
  (cond ((= b 0) n)
    ((even? b)(mul-iter (double a)(halve b) n))
    (else (mul-iter a (- b 1)(+ a n)))))

(mul 5 9)