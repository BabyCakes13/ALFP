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
            (begin
              (printf "car:~a cdr:~a acc:~a ~n." (car sl) (cdr sl) acc)
              (if (and (eq? (car sl) s) (not acc))
                  (cdr sl)
                  (cons (remove-first-acc (car sl) s #f) (remove-first-acc (cdr sl) s #f)))))))
  (trace remove-first-acc)
    (remove-first-acc sl s #t))

;(trace remove-first)
;(remove-first '(a b (a b)) 'b)

; 10. Define the function (notate nlst) which replaces every occurrence of asymbol s in a nested list nlst with the
; list (list s  d), where d is the nesting depth of the occurrence of s in nlst. For example:
; (notate ’(a b (() (a)))): ((a 1) (b 1) (() ((a 3))))
(define (notate nlst)
  (define (notate-acc nlst depth)
    (if (null? nlst)
        '()
        (if (not (list? nlst))
            (if (symbol? nlst)
                (list nlst depth)
                nlst)
            (cons (notate-acc (car nlst) (+ 1 depth)) (notate-acc (cdr nlst) depth)))))
  (trace notate-acc)
  (notate-acc nlst 0))

;(trace notate)
;(notate '(a b (() (a))))
;(notate '(1 b (b (b))))

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
;(btsize bt) ; this will yeld 7, vecause we have a, b, c, 1, 2, 3, 4.
(btdepth bt) ; this will yeld 3, since we have the depth (counting from 1) 3.