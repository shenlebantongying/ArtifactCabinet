#lang racket

;; x is what x?

;; macro binder, the val will be bind to id when eval in-x
(define-syntax-rule (in-x id val e)
  (let ([id val]) e))

(define-syntax-rule (out-x id val e)
  e)

; Anything other than #f is true
(define x '(3 4))

(in-x x '(1 2) (car x))  ;; -> 1 !! x in e is bind to val
  
(out-x x '(1 2) (car x)) ;; -> 3
