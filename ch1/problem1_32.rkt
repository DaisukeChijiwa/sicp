#lang racket
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner
      (term a)
      (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

;;sum-integers
(define (sum-integers a b)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (sum identity a inc b))

(sum-integers 1 10)

(define (sum-cubes a b)
  (define (cube x) (* x x x))
  (define (inc x) (+ x 1))
  (sum cube a inc b))

(sum-cubes 1 10)

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial n)
  (define (inc x) (+ x 1))
  (define (term i) i)
  (product term 1 inc n))

(factorial 10)

(define (accumulate2 combiner null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
    result
    (accumulate-iter (next a)
      (combiner (term a) result))))
  (accumulate-iter a null-value))

(define (product2 term a next b)
  (accumulate2 * 1 term a next b))

(define (factorial2 n)
  (define (inc x) (+ x 1))
  (define (term i ) i)
  (product2 term 1 inc n))

(factorial2 10)