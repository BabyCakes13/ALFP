#lang racket
(require racket/trace)

; The deep reverse of a list is the list produced by reversing the elements of all sublists of a list.
; Implement recursively (deep-reverse l) which returns the deep revers of list l.  For example:
; (deep-reverse ’((a b) (2 (3 4))): (((4 3) 2) (b a))
(define (deep-reverse l)
  (if (null? l)
      '()
      (if (not (list? l))
          l
          (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))
           
;(trace deep-reverse)
(deep-reverse '((a b c) (1 2 3)))
(deep-reverse '((a b) (2 (3 4))))