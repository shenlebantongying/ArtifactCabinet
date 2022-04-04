#lang racket

;; k -> continuation
(define (append-k lst1 lst2 k)
  (if  (empty? lst1)
       (k lst2)
       (append-k (rest lst1)
                 lst2
                 (λ (x) (k (cons (first lst1) x))))))

(append-k (list 1 2 3) (list 4 5 6) (lambda (x) x))
