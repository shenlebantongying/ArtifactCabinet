#lang racket

;; This program will convert the external rep -> internal

(module+ test
  (require rackunit))

;; external representation
;; VEJ = Integer | [VEJ,"+",VEJ] | [VEJ,"+",VEJ]
;;               | ["let",String,VEJ,AEJ] | String

;; internal representation
(struct node [op left right] #:transparent)
(struct decl [variable value scope] #:transparent)
; VE  = Integer | (node + VE VE) | (node * VE VE)
;     | (decl String VE VE) | String

; example let ["let", "x", ["x", "+", "x"]]

(define (parse aej)
  (cond
    [(number? aej) aej]
    [(string? aej) aej]
    [(list? aej)
     (match aej
       [`(,left "+" ,right) (node + (parse left) (parse right))]
       [`(,left "*" ,right) (node * (parse left) (parse right))]
       [`("let" ,(? string? x) ,value ,scope) (decl x (parse value) (parse scope))])]))

(module+ test
  (check-equal? (parse '(1 "+" 1)) (node + 1 1))
  (check-equal? (parse '("let" "x" 5 ("x" "+" "x")))
                (decl "x" 5 (node + "x" "x")))
  (check-equal? (parse '(3 "*" ("let" "x" 5 (1 "+" "x"))))
                (node * 3 (decl "x" 5 (node + 1 "x")))))
