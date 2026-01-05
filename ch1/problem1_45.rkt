#lang racket

(define (average x y)
  (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
    (lambda (x) x)
    (compose f (repeated f (- n 1)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
         (if (close-enough? guess next)
             next
             (try next))))
  (try first-guess))

(define (n-th-sqrt x n c)
  (fixed-point ((repeated average-damp c) (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

(n-th-sqrt (expt 3 3) 3 1)

(n-th-sqrt (expt 3 4) 4 2)

(define (log2 n)
  (/ (log n) (log 2)))

(define (n-th-sqrt-log x n)
  (define c (max 0 (inexact->exact (floor (log2 n)))))
  (fixed-point ((repeated average-damp c) (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

(n-th-sqrt-log (expt 3 3) 3)

(n-th-sqrt-log (expt 3 4) 4)