#lang racket

(define-struct opnode (op arg1 arg2) #:transparent)

(define (expand-binexp node)
  (cond
    [(number? node) (number->string node)]
    [else (string-append
           "("
           (expand-binexp (opnode-arg1 node))
           (opnode-op node)
           (expand-binexp (opnode-arg2 node))
           ")"
           )]))

(expand-binexp 42)
(expand-binexp (make-opnode "+" (make-opnode "+" 1 2) 3))
(expand-binexp (make-opnode "+" 1 (make-opnode "+" 2 3)))