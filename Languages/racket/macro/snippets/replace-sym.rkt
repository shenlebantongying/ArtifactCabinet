#lang racket

;; replace x with y in e
;; (replace-sym x y e)

;; v1 -> manually decompose syx
(define-syntax (replace-sym stx)
  (datum->syntax
   stx
   (let ([stx-list (syntax->datum stx)])
     (let ([x (cadr stx-list)]
           [y (caddr stx-list)]
           [e (cadddr stx-list)])
       (map (lambda (ele)
              (if (equal? ele x) y ele))
            e)))))

;; v2 -> uses quotation to simplify code
(define-syntax (replace-sym2 stx)
  (datum->syntax
   stx
   (let ([stx-list (syntax->datum stx)])
     `(map (lambda (ele)
             (if (equal? ele ,(cadr stx-list)) ,(caddr stx-list) ele))
           ,(cadddr stx-list)))))

;; v3 -> syntax-rules
(define-syntax replace-sym3
  (syntax-rules ()
    [(_ x y e)
     (map (lambda (ele)
            (if (equal? ele x) y ele))
          e)]))

(define x 1)
(define y 2)
(replace-sym x y (list x y))
(replace-sym2 x y (list x y))
(replace-sym3 x y (list x y))