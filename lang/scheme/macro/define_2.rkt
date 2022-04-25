#lang racket

(require (for-syntax syntax/parse))

(define-syntax define2
  (lambda (stx)                         ; match stx
    (syntax-parse stx
      [(_define2 var1 var2 expr)        ; to this patter
       (syntax                          ; produce new syntax obj
        (begin
          (define var1 expr)
          (define var2 var1)))])))


(define2 x y 12)

(list x y)
