#lang racket

;; TODO: learn ellipses
(define-syntax shift-to
  (syntax-rules ()
    ((shift-to (from0 from ...) (to0 to ...))
     (let ((tmp from0))
       (set! to from) ...
       (set! to0 tmp)))))

(define-syntax rotate
  (syntax-rules ()
    ((rotate a c ...)
     (shift-to (c ... a) (a c ...)))))

(begin
  (define a 1)
  (define b 2)
  (define c 3)
  (define d 4)

  (rotate a b c d)
  (values a b c d)
)
