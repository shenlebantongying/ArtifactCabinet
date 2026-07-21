#lang racket

;; Arbitrary number of rotate

;; WHY shift-to NEEDED?

;; TODO: try combine shift-to and rotate and u will know

(define-syntax shift-to
  (syntax-rules ()
    [(shift-to (to0 to ...) (from0 from ...))
     (let ([tmp from0])
       (set! to from) ...
       (set! to0 tmp))]))

(define-syntax rotate
  (syntax-rules ()
    [(rotate a b ...) (shift-to (a b ...) (b ... a))]))

(let ([a 1]
      [b 2]
      [c 3]
      [d 4])
  (rotate a b c d)
  (list a b c d))
