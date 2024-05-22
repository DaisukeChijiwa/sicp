#lang racket
(define (square n) (* n n))

(define (inc x) (+ x 1))

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

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (filtered-accumulate combiner null-value filter-pre term a next b)
  (cond ((> a b) null-value)
    ((filter-pre a)
     (combiner
       (term a)
       (filtered-accumulate combiner null-value filter-pre term (next a) next b)))
     (else (filtered-accumulate combiner null-value filter-pre term (next a) next b))))

(define (sum-squared-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

(sum-squared-primes 1 10)

(define (product-with-gcd n)
  (define (term i) i)
  (define (p i)
    (if (and (> n i) (= (gcd i n) 1))
      #t
      #f))
  (filtered-accumulate * 1 p term 1 inc n))

(product-with-gcd 10)

(define (filtered-accumulate-re combiner null-value filter-pre term a next b)
  (define (filtered-accumulate-iter a result)
    (cond ((> a b) result)
      ((filter-pre a)
       (filtered-accumulate-iter (next a)
         (combiner (term a) result)))
       (else (filtered-accumulate-iter (next a) result))))
  (filtered-accumulate-iter a null-value))

(define (sum-squared-primes-re a b)
  (filtered-accumulate-re + 0 prime? square a inc b))

(sum-squared-primes-re 1 10)

(define (product-with-gcd-re n)
  (define (term i) i)
  (define (p i)
    (if (and (> n i) (= (gcd i n) 1))
      #t
      #f))
  (filtered-accumulate-re * 1 p term 1 inc n))

(product-with-gcd-re 10)