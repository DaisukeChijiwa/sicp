#lang racket
(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(define (factorial n)
  (define (next i) (+ i 1))
  (define (term i) i)
  (product term 1 next n))

(factorial 10)

(define (even? n)
  (= (remainder n 2) 0))

(define (pi1 n)
  (define (next i) (+ i 1))
  (define (f k)
    (if (even? k)
      (/ (+ k 2) (+ k 1))
      (/ (+ k 1) (+ k 2))))
  (* 4.0 (product f 1 next n)))

(pi1 10)
(pi1 100)
(pi1 1000)

(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (pi2 n)
  (define (next i) (+ i 1))
  (define (f k)
    (if (even? k)
      (/ (+ k 2) (+ k 1))
      (/ (+ k 1) (+ k 2))))
  (* 4.0 (product2 f 1 next n)))

(pi2 10)
(pi2 100)
(pi2 1000)