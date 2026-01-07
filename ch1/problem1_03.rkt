#lang racket
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

(define (max-one a b c)
  (max a (max b c)))  ; 組み込みの max 関数を使用して最大値を計算

(define (second-one a b c)
  (if (= (max-one a b c) a)
      (max b c)
      (if (= (max-one a b c) b)
          (max a c)
          (max a b))))

; a, b, c の値を定義して関数を呼び出す
(define a 2)
(define b 3)
(define c 4)

; sum-of-squares に最大値と2番目の値を渡す
(sum-of-squares (max-one a b c) (second-one a b c))
