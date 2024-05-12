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
  
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (search-for-primes-iter a b))

(define (search-for-primes-iter a b)
  (when (< a b)
      (and (when (fast-prime? a 1)
        (timed-prime-test a))
        (search-for-primes-iter (+ a 1) b))))

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
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else #f)))



(search-for-primes 1000 1100)
(search-for-primes 10000 10100)
(search-for-primes 100000 100100)
(search-for-primes 1000000 1000100)