#lang racket
; Consider the set of nested lists defined inductively by
;〈nlist〉::= null | (cons〈symbol〉 〈nlist〉)| (cons〈number〉 〈nlist〉) | (cons〈nlist〉 〈nlist〉)
; and the set of substitutions defined recursively by
; 〈subst〉::= null | ((cons (list〈symbol〉 〈value〉)〈subst〉)
; where〈value〉can be any value.  Define recursively the following functions:

; (a)  The recogniser functions nlist? for〈nlist〉, and subst? for substitutions.
(define (nlist? l)
  (cond [(null? l) #t]
        ; if the car of the list is not a list itself, we check whether it is symbol or number.
        [(not (list? (car l)))
         (if (or (symbol? (car l)) (number? (car l)))
             (nlist? (cdr l))
             #f)]
        ; if the head of the list is a list, then we move on to its tail and check the found elements.
        [(list? (car l))
         (nlist? (cdr l))]))

(define (subst? l)
  (if (null? l)
      #t
      (if (and (list? (car l))
               (= (length (car l)) 2)
               (symbol? (caar l))
               (number? (cadar l)))
          (subst? (cdr l))
          #f)))
          
;(trace nlist?)
;(trace subst?)
;(nlist? '(1 s (a s b) s c #t)) ; false, because #t is neither a symbol, nor a number.
;(nlist? '(1 s (a s b) s c d)) ; true, because all the elements are symbols, numbers, or lists of symbols and numbers.
;(subst? '((a 3) (b 4))) ; true, because all the conditions are satisfied;
; the rest will be false from various conditions.
;(subst? '((a a) (b 4)))
;(subst? '((a 3) (4 4)))
;(subst? '((a 3 4) (b 4)))
;(subst? '(a (a a) (b 4)))

; (b) The  function (removeAll nlst s) which removes all occurrences of symbols from the nested list nlst.
; For example, (removeAll ’(1 s (a s b) s c) ’s) should return ’(1 (a b) c).
(define (removeAll nlst s)
  (if (null? nlst)
      '()
      (if (not (list? nlst))
              nlst
          (if (eq? (car nlst) s)
              (removeAll (cdr nlst) s)
              (cons (removeAll (car nlst) s) (removeAll (cdr nlst) s))))))
          
;(trace removeAll)
;(removeAll '(1 s (a s b) s c) 's)

; (c) The function (get subst s) which returns the value paired with symbols in the substitution subst, if it exists,
; and s is s is not paired with any value in subst. For example:
; (get '((a 4) (b 6)) ’b): 6
; (get '((a 4) (b 6)) ’c): 'c
(define (get subst s)
  (if (null? subst)
      s
      (if (eq? (caar subst) s)
          (cadar subst)
          (get (cdr subst) s))))

;(get '((a 4) (b 6)) 'b) ; this will return 6, since the substitution is found.
;(get '((a 4) (b 6)) 'c) ; this will return the original symbol, since no substitution could be found.

; (d) The function (substitute nlst subst) which computes the nestedlist in which every symbols
; from nlst is replaced with (get subst s). For example:
; (substitute ’(s (a ((b)) s (c)) ’((s 1) (b 3))): ’(1 (a ((3)) 1 (c))
(define (substitute nlst subst)
  (if (null? nlst)
      '()
      (if (not (list? nlst))
          (if (symbol? nlst)
              (get subst nlst)
              nlst)
          (cons (substitute (car nlst) subst) (substitute (cdr nlst) subst)))))

;(trace get)
;(trace substitute)
;(substitute '(s a b)
            ;'((s 1) (b 3)))
;(substitute '(s (a ((b)) s (c)))
            ;'((s 1) (b 3)))

; The function (countsym nlst) which counts the number of symbol occurrences in the nested list nlst.
; For example: (countsym ’((s (1 a) b (c d) s))): 6.
(define (countsym nlst)
  (if (null? nlst)
      0
      (if (not (list? nlst))
          (if (symbol? nlst)
              1
              0)
          (+ (countsym (car nlst)) (countsym (cdr nlst))))))

;(trace countsym)
;(countsym '((s (1 a) b (c d) s)))

(printf "~nExercice 8 (a).~n")
(nlist? '(1 s (a s b) s c #t)) ; false, because #t is neither a symbol, nor a number.
(nlist? '(1 s (a s b) s c d)) ; true, because all the elements are symbols, numbers, or lists of symbols and numbers.

(subst? '((a 3) (b 4))) ; true, because all the conditions are satisfied;
(subst? '((a a) (b 4))) ; false, because we cannot have (symbol . symbol)
(subst? '((a 3) (4 4))) ; false, because we cannot have (number . number)
(subst? '((a 3 4) (b 4))) ; false, because we cannot have (item item item)
(subst? '(a (a a) (b 4))) ; false, because the length of all nested lists must be 2 in a substitution datatype.

(printf "~nExercice 8 (b).~n")
(removeAll '(1 s (a s b) s c) 's) ; will remove each occurence of 's, if found.
(removeAll '(1 s (a s b) s c) 'd) ; will not remove anything, since 'd cannot be found.

(printf "~nExercice 8 (c).~n")
(get '((a 4) (b 6)) 'b) ; this will return 6, since the substitution is found.
(get '((a 4) (b 6)) 'c) ; this will return the original symbol, since no substitution could be found.

(printf "~nExercice 8 (d).~n")
; the results are self explanatory here.
(substitute '(s a b)
            '((s 1) (b 3)))
(substitute '(s (a ((b)) s (c)))
            '((s 1) (b 3)))

(printf "~nExercice 8 (e).~n")
(countsym '((s (1 a) b (c d) s))) ; this will yeld 6, since we have 6 symbols present.