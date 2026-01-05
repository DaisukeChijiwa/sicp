#lang racket

(define dx 0.00001)

(define (smooth f )
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define (n-smooth f n)
  ((repeated smooth n) f))

(define (square a)
  (* a a))

((smooth square) 2)

((n-smooth square 5) 2)