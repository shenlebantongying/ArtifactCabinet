#lang racket

;; Navie rotate3
(define-syntax swap
  (syntax-rules ()
    [(swap x y)
     (let ([tmp x])
       (set! x y)
       (set! y tmp))]))

(define-syntax rotate3
  (syntax-rules ()
    [(rotate a b) (swap a b)]
    [(rotate a b c)
     (begin
       (swap a b)
       (swap b c))]))

(let ([x 1]
      [y 2]
      [z 3])
  (rotate3 x y z)
  (list x y z))
