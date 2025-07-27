#lang racket

(require racket/fixnum)

(struct Int (value))
(struct Prim (op args))
(struct Program (info body))

(define (interp_exp e)
  (match e

    [(Int n) n]

    [(Prim 'read '())
     (define r (read))
     (cond
       [(fixnum? r)]
       [else (error 'interp_exp "read expecting an int" r)])]

    [(Prim '- (list e))
     (define v (interp_exp e))
     (fx- 0 v)]
    [(Prim '+ (list e1 e2))
     (define v1 (interp_exp e1))
     (define v2 (interp_exp e2))
     (fx+ v1 v2)]
    [(Prim '- (list e1 e2))
     (define v1 (interp_exp e1))
     (define v2 (interp_exp e2))
     (fx- v1 v2)]))

(define (interp_Lint p)
  (match p
    [(Program '() e) (interp_exp e)]))

(interp_Lint (Program '() (Prim '+ (list (Int 10) (Int 32)))))
