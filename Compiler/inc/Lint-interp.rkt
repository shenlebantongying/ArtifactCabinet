#lang racket
(require racket/fixnum)

(struct Int (value))
(struct Prim (op args))
(struct Program (info body))

;; sample program
#;(Program '() (Prim '+ (1 2)))

(define (interp_exp e)
  (match e
    [(Int n) n]
    [(Prim '+ (list e1 e2))
     (let ([v1 (interp_exp e1)]
           [v2 (interp_exp e2)])
       (fx+ v1 v2))]))

(define (interp_Lint p)
  (match p
    [(Program '() e) (interp_exp e)]))

(interp_Lint
 (Program '() (Prim '+ (list (Int 3) (Prim '+ (list (Int 1) (Int 2)))))))
