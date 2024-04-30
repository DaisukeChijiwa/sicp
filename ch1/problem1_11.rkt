#lang racket
; for n < 3, f(n) = n, n >= 3, f(n) = f(n - 1) + 2f(n - 2) + df(n - 3)
; first write recursive, then iterative
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) 
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f-iterative n)
  (define (iter i fn2 fn1 fn0)
    (if (= i n)
        fn0
        (iter (+ i 1) fn1 fn0 (+ fn0 (* 2 fn1) (* 3 fn2)))))
  (cond ((< n 3) n)  ; nが2以下の場合は n を直接返す
        (else (iter 2 0 1 2))))  ; nが3以上の場合は、i を 2 から始める



(display (f-iterative 4))  ; 出力: 11
(newline)
(display (f-iterative 5))  ; 出力: 25
(newline)
