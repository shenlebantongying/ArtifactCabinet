#lang racket
(require racket/trace)

;; fold to get min value

(define (get-smaller a b) (if (< a b) a b))

(trace get-smaller)

(foldr get-smaller
       9999999
       (list 3 9 8 7 1 2))
