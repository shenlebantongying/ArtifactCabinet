#lang racket

(struct node (op l r) #:transparent)
(struct decl (var val scope) #:transparent)

(define (sub var val expr)
  (match expr
    [(? integer?) expr]
    [(? string?) (if (eq? var expr) val expr)]
    [(node op l r) (node op (sub var val l) (sub var val r))]
    [(decl var2 val2 scope)
     ;; name conflict happens
     (if (eq? var var2)
         ;; this means we only change the
         (decl var2 (sub var val val2) scope)
         (decl var2 (sub var val val2) (sub var val scope)))]))

;; Note about name conflict, consider:
;; Outer scope (sub x -> 5)
;; in this situation (let ([x x]) x)
;; the x is bind to the value of outer x

(define (run expr)
  (match expr
    [(? integer?) expr]
    [(node op l r) (op (run l) (run r))]
    [(decl var val scope) (run (sub var (run val) scope))]))

(and (equal? 3 (run (decl "x" 1 (decl "y" 2 (node + "x" "y")))))
     ;; x will be value most close to the calculation place
     (equal? 20 (run (decl "x" 5 (decl "x" 10 (node + "x" "x"))))))
