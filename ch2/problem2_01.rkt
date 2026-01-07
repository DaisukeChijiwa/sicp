#lang racket

(define (make-rat n d)
  (let* ((g (gcd (abs n) (abs d)))
          (n1 (/ n g))
          (d1 (/ d g)))
          (if (< d1 0)
            (cons (- n1) (- d1))
            (cons n1 d1))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

(define neg-one-third (make-rat 1 -3))

(print-rat (add-rat neg-one-third one-half))

(print-rat (add-rat one-third one-third))

(print-rat (sub-rat one-third one-half))

(print-rat (mul-rat one-third neg-one-third))

(print-rat (div-rat one-third one-half))