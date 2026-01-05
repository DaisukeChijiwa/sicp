#lang racket

(define (double f)
  (lambda (x) (f (f x))))

(define (square a)
  (* a a))

(define (inc a)
  (+ a 1))

((double inc) 1)

((double square) 2)

(((double (double double)) inc) 5)