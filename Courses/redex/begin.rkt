#lang racket
(require redex)

(define-language book-any-lang
  [B true
     false
     (V B B)]
  [C (V C B)
     (V B C)
     hole])

(define B1 (term true))
(define B2 (term false))
(define B3 (term (V true false)))
(define B4 (term (V ,B1 ,B2)))
(define B5 (term (V false ,B4))) ;;=> '(V false (V true false))

(redex-match book-any-lang
             B
             (term (V false true)))