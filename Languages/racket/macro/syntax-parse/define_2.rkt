#lang racket

(require (for-syntax syntax/parse))

(define-syntax (define2 stx)
  (syntax-parse stx
    [(_ var1:identifier var2:identifier expr:expr)     
     #'(begin 
         (define var1 expr)
         (define var2 var1))]))

(define2 x y 12)
(list x y)


