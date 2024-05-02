#lang racket
(define(count-change amount)
  (cc amount 5))

(define (cc amount coin-type)
  (cond ((= amount 0) 1)
    ((or (< amount 0) (= coin-type 0)) 0)
    (else (+ (cc amount
        (- coin-type 1))
      (cc (- amount
          (first-denom coin-type))
        coin-type)))))

(define (first-denom coin-type)
  (cond ((= coin-type 1) 1)
    ((= coin-type 2) 5)
    ((= coin-type 3) 10)
    ((= coin-type 4) 25)
    ((= coin-type 5) 50)))

(count-change 11)