#lang racket
(require (for-syntax syntax/parse))

;; complete implementation of
;; https://lexi-lambda.github.io/racket-macro-exercises/threading-2.html

(define-syntax (~> stx)
  (syntax-parse stx
    #:datum-literals (@)
    [(_ x) #'x]                       ;; rule 1

    [(_ x (f a ... @ b ...) more ...)   ;; rule 4
     #'(~> (f a ... x b ...) more ...)] ;; note rule4 must preced rule3 due to similarity

    [(_ x (f a ...) more ...)
     #'(~> (f x a ...) more ...)]       ;; rule 2

    [(_ x id more ...)
     #'(~> x (id) more ...)]            ;; rule 3
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

;rule 4
(~> 1 (list-ref '(10 20) @))

(~> '(1 2 3)
      (map add1 @)
      (apply + @)
      (- 1))
