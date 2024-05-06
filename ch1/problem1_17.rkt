#lang racket
(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (log-step-mul a b)
  (cond ((= b 0) 0)
    ((even? b) (log-step-mul (double a)(halve b)))
    (else (+ (log-step-mul (double a)(halve (- b 1))) a))))

(log-step-mul 5 9)