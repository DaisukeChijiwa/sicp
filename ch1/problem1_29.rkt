#lang racket
(define (inc x) (+ x 1))

(define (cube x) (* x x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
      (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
    dx))

(integral cube 0 1 100)

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k ) (f (+ a (* k h))))
  (define (next i) (+ i 1))
  (define (term i)
    (* (if (even? i) 2 4)
      (y i)))
    (* (/ h 3.0)
      (+ (y 0)
        (y n)
        (sum term
          1
          next
          (- n 1)))))

(simpson-integral cube 0 1 100)