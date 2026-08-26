#lang racket
(require rackunit)
(require "sicp-deriv.rkt")

(check-equal? (product? (list 'c 1 1)) #f)

(check-equal? (make-product 0 'x) 0)

(check-equal? (deriv 'x 'x) 1)
(check-equal? (deriv (list '+ 'x 1) 'x) 1)
(check-equal? (deriv (list '* 'x 1) 'x) 1)
(check-equal? (deriv (list '+ (list '+ 'x 'x) 1) 'x) 2)

(check-equal? (deriv (list '* 'x 'x) 'x) (list '+ 'x 'x))
