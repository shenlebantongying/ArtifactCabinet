#lang racket
(require racket/trace)
  
(define (insert x acc)
  (cond
    [(empty? acc) (list x)]
    [(< x (car acc)) (cons x acc)]
    [else (cons (car acc) (insert x (cdr acc)))]))

(trace insert)

(foldr insert '() (list 5 3 1 4 2))
