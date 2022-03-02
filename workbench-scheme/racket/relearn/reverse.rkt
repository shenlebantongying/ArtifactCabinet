#lang racket
(require racket/trace)

;; acc => accumulator
(define (rev lst acc)
  (cond 
    [(empty? lst) acc]
    [else (rev (rest lst) (cons (car lst) acc))]))

(define (my-reverse lst)
  (rev lst '()))

(trace rev)

(my-reverse (list 1 2 3 4 5 6))
