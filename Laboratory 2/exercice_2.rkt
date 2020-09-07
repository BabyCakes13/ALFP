#lang racket
(require racket/trace)

;If n is a positive integer then(sum-digits n) computes the sum of its digits.  For example;
;(sum-digits 234): 9. (sum-digits 5) 5.
(define (sum-digits n)
  (if (= n 0)
      0
      (+ (remainder n 10) (sum-digits (/ (- n (remainder n 10)) 10)))))

; This is the tail-recursive version of the function. It is tail-recursive because the function
; (sum-digits-tr-acc n acc) is tail-recursive.
(define (sum-digits-tr n)
  (define (sum-digits-tr-acc n acc)
    (if (= n 0)
        acc
        (sum-digits-tr-acc (/ (- n (remainder n 10)) 10) (+ acc (remainder n 10)))))
  (trace sum-digits-tr-acc)
  (sum-digits-tr-acc n 0))

;(trace sum-digits)
;(trace sum-digits-tr)
(sum-digits 2433)
(sum-digits-tr 2433)