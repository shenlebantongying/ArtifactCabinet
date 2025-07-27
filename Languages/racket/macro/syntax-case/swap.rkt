#lang racket

;; #'template is equal to (syntax template)

(define-syntax (swap stx)
  (syntax-case stx ()
    [(swap x y)
     (if (and (identifier? #'x) (identifier? #'y))
         (syntax (let ([tmp x])
                   (set! x y)
                   (set! y tmp)))
         (raise-syntax-error #f "not an identifier" stx #'y #'x))]))

(define x 2)
(define y 1)

(swap x y)

(values x y)
