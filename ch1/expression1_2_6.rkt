#lang racket
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 1007)
(smallest-divisor 10007)
(smallest-divisor 10000007)
(smallest-divisor 1000000007)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
  ((even? exp)
   (remainder (square (expmod base (/ exp 2) m))
              m))
  (else
   (remainder (* base (expmod base (- exp 1) m))
              m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n ) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (even? n)
  (= (remainder n 2) 0))

(fermat-test 1007)