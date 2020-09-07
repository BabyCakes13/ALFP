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

;(fact 5)

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
;(sum-digits 2433)
;(sum-digits-tr 2433)

; The function call(increasing-digits? n) expects a positive integern as input, and should return
; #t if n is a sequence of digits in increasing order (e.g.,5,579,445677), and #f otherwise.
(define (increasing-digits? n)
  (if (= n 0)
      #t
      (if (< (remainder n 10) (quotient (remainder n 100) 10))
          #f
          (increasing-digits? (quotient (- n (remainder n 10)) 10)))))
;(trace increasing-digits?)
;(increasing-digits? 123)
;(increasing-digits? 132)
;(increasing-digits? 4456)

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
;(alternating01 '())
;(alternating01 '(0 1 0))
;(alternating01 '(1 0 1))
;(alternating01 '(1 1 0))
;(alternating01 '(1 0 0))

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
;(deep-reverse '((a b c) (1 2 3)))
;(deep-reverse '((a b) (2 (3 4))))

; Define recursively the function (deep-symbol->string l) which replaces all symbols that occur in list l,
; at any depth, into strings. Use the pre-defined function symbol-string which converts a symbol into the
; corre-sponding string, and the predicate symbol? which recognises symbols. For example:
; (deep-symbol->string ’(a b (ab a (c 1 2) ()))): ’("a" "b" ("ab" "a" ("c" 1 2) ())))

(define (deep-symbol->string l)
  (if (null? l)
      '()
      (if (not (list? l))
          (if (symbol? l)
              (symbol->string l)
              l)
          (append (list (deep-symbol->string (car l))) (deep-symbol->string (cdr l))))))

; (trace deep-symbol->string)
; (deep-symbol->string '(a b (ab a (c 1 2) ())))

; Write a tail-recursive version of the function power-sum such that the function call (power-sum x n) computes
; the sum 1 + x +. . . +x^n for anynumber x and integer n≥0. In the base case when n= 0, the function call return 1.
(define (power-sum x n)
  (define (power-sum-acc x n acc)
    (if (= n 0)
        (+ acc 1)
        (power-sum-acc x (- n 1) (+ (expt x n) acc))))
  (trace power-sum-acc)
  (power-sum-acc x n 0))

;(trace power-sum)
;(power-sum 2 4)

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

; The  function (removeAll nlst s) which removes all occurrences of symbols from the nested list nlst.
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

; c) The function (get subst s) which returns the value paired with symbols in the substitution subst, if it exists,
; and s is s is not paired with any value in subst. For example:
; (get '((a 4) (b 6)) ’b): 6
; (get '((a 4) (b 6)) ’c): 'c
(define (get subst s)
  (if (null? subst)
      s
      (if (eq? (caar subst) s)
          (cadar subst)
          (get (cdr subst) s))))

(get '((a 4) (b 6)) 'b)
(get '((a 4) (b 6)) 'c)