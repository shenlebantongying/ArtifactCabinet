#lang racket
(require racket/trace)
;; remove duplicate or list->set

(define (member* x acc)
  (cond
    [(empty? acc) (list x)]
    [(member x acc)  acc]
    [else (cons x acc)]))

(trace member*)

(foldr member* '() (list 1 1 2 2 3 3 2 4))
