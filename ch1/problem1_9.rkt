#lang racket
; linear recursive process
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
      
(+ 4 5)
(inc (+ (dec 4) 5))
(inc (inc (+ (dec3) 5)))
(inc (inc (inc (+ (dec2) 5))))
(inc (inc (inc (inc (+ (dec1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc6)))
(inc (inc7))
(inc 8)
9

; linear iterative process
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)
9 ; a = 0