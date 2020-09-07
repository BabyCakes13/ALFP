#lang racket
(require racket/trace)

; Write a tail-recursive version of the function fact, where(fact n) computes the factorial of n.
; This function is considered tail-recursive because (fact n) = (fact-acc n acc), and the last function
; is tail-recursive (because the last call represents the recursive function itself.
(define (fact n)
  (define (fact-acc n acc)
    (if (= n 0)
        acc
        (fact-acc (- n 1) (* acc n))))
  (trace fact-acc)
  (fact-acc n 1))

(fact 5)