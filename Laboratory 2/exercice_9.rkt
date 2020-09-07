#lang racket
(require racket/trace)

; Consider the set of lists of symbols defined inductively by
; 〈slist〉::= null | (cons〈symbol〉 〈slist〉)
; Define recursively the following functions:

; (a)(slist? sl) returns #t if slis a list of symbols, and #f otherwise.
(define (slist? sl)
  (if (null? sl)
      #t
      (if (list? sl)
          (and
            (slist? (car sl))
            (slist? (cdr sl)))
          (if (symbol? sl)
              #t
              #f))))
;(trace slist?)
;(slist? '(a b (a b)))

; (b)(remove-all sl s) removes all occurrences of symbol s from the list sl.

(define (remove-all sl s)
  (if (null? sl)
      '()
      (if (not (list? sl))
; since in the next if we jump over any s, we will never get to the point when we have a single element which is s.
          sl
          (if (eq? (car sl) s)
              (remove-all (cdr sl) s)
              (cons (remove-all (car sl) s) (remove-all (cdr sl) s))))))
;(remove-all '(a b (a b)) 'a) ; this will return '(b (b)).
;(remove-all '(a b (a b)) 'c) ; since no c is found, the original list will be returned.

; (c)(remove-first sl s) removes the first occurrence of symbol s from the list of symbols sl.
(define (remove-first sl s)
  (define (remove-first-acc sl s acc)
    (if (null? sl)
        '()
        (if (not (list? sl))
            sl
              (if (and (eq? (car sl) s) (not acc))
                  (cdr sl)
                  (cons (remove-first-acc (car sl) s #f) (remove-first-acc (cdr sl) s #f))))))
    (remove-first-acc sl s #t))

;(trace remove-first)
;(remove-first '(a b (a b)) 'b)

(printf "~nExercice 9 (a).~n")
(slist? '(a b (a b)))
(slist? '(a b (2 b)))

(printf "~nExercice 9 (b).~n")
(remove-all '(a b (a b)) 'a) ; this will return '(b (b)).
(remove-all '(a b (a b)) 'c) ; since no c is found, the original list will be returned.

(printf "~nExercice 9 (c).~n")
(remove-first '(a b a b) 'b)