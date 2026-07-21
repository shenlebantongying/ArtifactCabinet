#lang racket

;; get all permutations with 2 elements

;; Note how we uses a helper to combine 2 elements at a time

(define-syntax shift
  (syntax-rules ()
    [[_ (a1 a2 ...) (b1 b2 ...)] (list (list a1 b1) (list a2 b2) ...)]))

(define-syntax permutation2
  (syntax-rules ()
    [[group2 a b ...] (shift (a b ...) (b ... a))]))

(permutation2 1 2 3 4 5)
