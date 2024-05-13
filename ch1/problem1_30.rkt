#lang racket
(define (sum-linear-recursion term a next b)
  (if (> a b)
    0
    (+ (term a)
      (sum-linear-recursion term (next a) next b))))

(define (sum-tail-recursion term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
    (iter a 0))

(define (sum-integers-linear a b)
  (if (> a b)
      0
      (+ a (sum-integers-linear (+ a 1) b))))

(sum-integers-linear 1 10)

(define (sum-integers-tail a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (+ a 1) (+ a result))))
  (iter a 0))

(sum-integers-tail 1 10)
