#lang racket
; 11. Consider the set of binary trees defined inductively by
; 〈btree〉::=〈number〉| (list〈symbol〉 〈btree〉 〈btree〉)
; The size of a binary tree bt ∈〈btree〉is the number of numbers and symbols that occur in bt.
; The depth of a binary tree is the number of nodes along the longest path from the root to a leaf node.

; a)  Define recursively the following functions:
; i. (btsizebt) which computes the size of the binary tree bt.

(define (btsize bt)
  (if (null? bt)
      0
      (if (not (list? bt))
          (if (or (number? bt) (symbol? bt))
              1
              0)
          (+ (btsize (car bt)) (btsize (cdr bt))))))

; ii.(btdepth bt) which computes the depth of the binary tree bt.
(define (btdepth bt)
  (cond [(null? bt) 0] ; if the list is null, its depths is 0.
        [(list? bt)
         (+ 1 (btdepth (cdr bt)))]  ; if we find a new list, we count the depth.
        [(not (list? bt)) 0])) ; if the element is not a list, we do not add additional depth.

;(trace btsize)
;(trace btdepth)
(define bt '(a (b 1 2)
               (c 3 4)))
;     a
;  b     c
;1  2   3  4

(btsize bt) ; this will yeld 7, vecause we have a, b, c, 1, 2, 3, 4.
(btdepth bt) ; this will yeld 3, since we have the depth (counting from 1) 3.