#lang racket

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (iter-a-to-b f a b)
  (when (<= a b)
    (newline)
    (display a)
    (display " -> ")
    (display (f a))
    (iter-a-to-b f (+ a 1) b)))

(iter-a-to-b
  (lambda (k)
          (+
            (cont-frac (lambda (i) 1.0)
                       (lambda (i) (if (= (remainder i 3) 2)
                                       (- i (/ (- i 2) 3))
                                       1))
                       k)
            2))
  1
  20)