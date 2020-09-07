#lang racket
(require racket/trace)

; Write a tail-recursive version of the function power-sum such that the function call (power-sum x n) computes
; the sum 1 + x +. . . +x^n for anynumber x and integer n≥0. In the base case when n= 0, the function call return 1.
(define (power-sum x n)
  (define (power-sum-acc x n acc)
    (if (= n 0)
        (+ acc 1)
        (power-sum-acc x (- n 1) (+ (expt x n) acc))))
  (trace power-sum-acc)
  (power-sum-acc x n 0))

(trace power-sum)
; 2^0 + 2^1 + 2^2 + 2^3 + 2^4)
(power-sum 2 4)