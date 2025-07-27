#lang racket

(define (two-sum lst)
  (cond
    [(= (length lst) 1) empty]
    [(cons (+ (first lst) (second lst)) (two-sum (rest lst)))]))

(define (tri lst counter)
  (cond
    [(zero? counter) (list lst)]
    [else (cons lst (tri (append '(1) (two-sum lst) '(1)) (sub1 counter)))]))

(tri '(1) 4)

(define (wrap n)
  (tri '(1) (sub1 n)))

(wrap 4)
