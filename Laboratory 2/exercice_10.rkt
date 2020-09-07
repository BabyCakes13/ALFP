#lang racket
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
  (notate-acc nlst 0))

;(trace notate)
(notate '(a b (() (a))))
(notate '(1 b (b (b))))