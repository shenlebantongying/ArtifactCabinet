;; Rework on SICP 2.3.2
;; TODO: SICP exercises 2.56

#lang racket
(provide (all-defined-out))

;(symbol? 'a)
;(eq? 'a 'a)

(define (number-eq-lambda? v)
  (lambda (e) (and (number? e) (zero? v))))

(define (make-sum a1 a2)
  (match (cons a1 a2)
    [(cons (? number?) (? number?)) (+ a1 a2)]
    [(cons (? (number-eq-lambda? 0)) a2) a2]
    [(cons a1 (? (number-eq-lambda? 0))) a1]
    [_ (list '+ a1 a2)]))

(define (make-product a1 a2)
  (match (cons a1 a2)
    [(cons 0 _) 0]
    [(cons _ 0) 0]
    [(cons (? number?) (? number?)) (* a1 a2)]
    [(cons (? number-eq-lambda? 1) a2) a2]
    [(cons a1 (? number-eq-lambda? 1)) a1]
    [_ (list '* a1 a2)]))

(define-syntax-rule (op-check-lambda sym)
  (match-lambda
    [(list sym _ _) #t]
    [_ #f]))

(define sum? (op-check-lambda '+))
(define product? (op-check-lambda '*))

(define op-left second)
(define op-right third)

;; for deriving f(x), var is x
(define (deriv expr var)
  (cond
    [(number? expr) 0]
    [(symbol? expr) (if (eq? expr var) 1 0)]
    [(sum? expr) (make-sum (deriv (op-left expr) var) (deriv (op-right expr) var))]
    [(product? expr)
     (make-sum (make-product (op-left expr) (deriv (op-right expr) var))
               (make-product (deriv (op-left expr) var) (op-right expr)))]))
