#lang racket

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (square a)
  (* a a))

(define (iter-a-to-b f a b)
  (when (<= a b)
    (newline)
    (display a)
    (display " -> ")
    (display (f a))
    (iter-a-to-b f (+ a 1) b)))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2 i) 1))
             k))

(iter-a-to-b
  (lambda (k) (tan-cf 1.0 k))
  1
  20)