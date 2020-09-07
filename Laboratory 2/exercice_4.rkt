#lang racket
(require racket/trace)

; Write down a tail recursive definition of the function alternating01 such that (alternating01 l) returns
; #t if l is a list of alternating 0 and 1, and #f otherwise. For example: (alternating01 ’())#t.
; (alternating01 ’(1 0 1)) #t. alternating01 ’(0 1 1 0)) #f.
(define (alternating01 l)
  (if (= (length l) 0)
      #t
      (if (= (length l) 1)
          #t
          (if (eq? (car l) (car (cdr l)))
              #f
              (alternating01 (cdr l))))))

;(trace alternating01)
(alternating01 '())
(alternating01 '(0 1 0))
(alternating01 '(1 0 1))
(alternating01 '(1 1 0))
(alternating01 '(1 0 0))