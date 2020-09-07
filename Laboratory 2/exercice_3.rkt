#lang racket
(require racket/trace)

; The function call(increasing-digits? n) expects a positive integern as input, and should return
; #t if n is a sequence of digits in increasing order (e.g.,5,579,445677), and #f otherwise.
(define (increasing-digits? n)
  (if (= n 0)
      #t
      (if (< (remainder n 10) (quotient (remainder n 100) 10))
          #f
          (increasing-digits? (quotient (- n (remainder n 10)) 10)))))

;(trace increasing-digits?)
(increasing-digits? 123)
(increasing-digits? 132)
(increasing-digits? 4456)