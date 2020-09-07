#lang racket
(require racket/trace)

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
(deep-symbol->string '(a b (ab a (c 1 2) ())))