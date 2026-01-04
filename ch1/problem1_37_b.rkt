#lang racket

(define (cont-frac-iter n d k)
  (define (loop i result)
    (if (= i 0)
        result
        (loop (- i 1) (/ (n i) (+ (d i) result)))))
  (loop k 0))

(define (iter-a-to-b f a b)
  (when (<= a b)
    (newline)
    (display a)
    (display " -> ")
    (display (f a))
    (iter-a-to-b f (+ a 1) b)))

(iter-a-to-b
  (lambda (k)
          (cont-frac-iter (lambda (i) 1.0)
                        (lambda (i) 1.0)
                        k))
  1
  20)