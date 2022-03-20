#lang racket
x
(module+ test (require rackunit))

;; Simple Arithmetic Expression Language
;; AE = Integer | (list AE '+ AE) | (list AE '* AE)

(define (calc expr)
  (match expr
    [(? number?) expr]
    [(list left-expr '+ right-expr) (+ (calc left-expr) (calc right-expr))]
    [(list left-expr '* right-expr) (* (calc left-expr) (calc right-expr))]))

(define ex1 42)
(define ex2 '(1 + 1))
(define ex3 '((1 + 2) * (4 + 5)))

(module+ test
  (check-equal? (calc ex1) 42)
  (check-equal? (calc ex2) 2)
  (check-equal? (calc ex3) 27)
)
