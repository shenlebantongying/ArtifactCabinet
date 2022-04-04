#lang racket
(require rackunit
         "all-bricks.rkt")

(check-equal?
 (slb-len '(cons 1 (cons 2 (cons 3))))
 3 
 "slb-len")

(check-equal?
 (add-comma)
 "A,  B,  C, "
 "add-comma")


