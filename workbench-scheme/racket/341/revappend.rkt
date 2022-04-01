#lang racket
(require rackunit)

;; fi w22 solved

(define (rev lst acc)
  (cond
    [(empty? lst) acc]
    [else (rev (rest lst) (cons (car lst) acc))]))

(define (revappend l1 l2)
  (append (rev l1 '()) l2))

(check-equal? (revappend (list 1 2 3) (list 4 5 6)) '(3 2 1 4 5 6))

(define (wrev lst)
  (revappend lst '()))

(check-equal? (wrev '(1 2 3 4)) (reverse '(1 2 3 4)))
