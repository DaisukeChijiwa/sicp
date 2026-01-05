#lang racket

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square a)
  (* a a))

(define tolerance 0.00001)

(define (iterative-improve enough? improve)
  (lambda (guess)
          (define (iter guess)
            (if (enough? guess)
                guess
                (iter (improve guess))))
          (iter guess)))

(define (sqrt x)
  (define (enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(sqrt 9)

(fixed-point cos 1.0)