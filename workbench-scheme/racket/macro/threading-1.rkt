#lang racket

; a better threading may refer
; https://github.com/lexi-lambda/threading

(define-syntax ~>
  (syntax-rules (V)
    [(~> x) x]                       ; rule1

    [(~> x (f a ...) more ...)
     (~> (f x a ...) more ...)]       ; rule2

    [(~> x id more ...)
     (~> x (id) more ...)]            ; rule3

   #;  [(~> x (f a ... _ b ...) more ...)
        (~> (f a ... x b ...) more ...)] ; rule4
    ; doesn't work due to ... 's limitation, see threading-2.rkt
    ))

; rule 1
(~> 'hello)
(~> (+ 1 2))
(~> (let ([x 12])
      (* x 3)))

; rule 2
(~> "Hello, "
    (string-append "world"))
(~> 1 (+ 3) (- 5))

;rule 3
(~> #\a
    (list #\b)
    list->string)
