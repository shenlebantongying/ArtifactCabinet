#lang racket
(require (for-syntax syntax/parse))

(define-syntax (mylet stx)
  (syntax-case stx ()
    [(_ ((var expr) ...) body)
     #'(let ((var expr) ...) body)]))

(define-syntax (mylet2 stx)
  (syntax-parse stx
    [(_ ([v:id e:expr] ...) body ...+)
     #'(let ([v e] ...) body ...)]))

(mylet ((x 1) (y 2)) (+ x y))
(mylet2 ((x 1) (y 2)) (+ x y))
