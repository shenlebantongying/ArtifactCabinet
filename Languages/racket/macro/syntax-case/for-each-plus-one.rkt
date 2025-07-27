#lang racket

(define-syntax for-each-plus-one
  (lambda (stx)
    (syntax-case stx ()
      [(_ e) #'(cons (+ e 1) '())]
      [(_ e1 e2 e3 ...) #'(cons (+ 1 e1) (for-each-plus-one e2 e3 ...))])))

(for-each-plus-one 1 2 3 4 5 6 7 8)

;; racket-y-ify

(define-syntax (v2 stx)
  (syntax-case stx ()
    [(_ e) #'(cons (+ e 1) '())]
    [(_ e1 e2 e3 ...) #'(cons (+ 1 e1) (v2 e2 e3 ...))]))

(v2 1 2 3 4 5 6 7 8)

;; define the same in syntax-rules

(define-syntax v3
  (syntax-rules ()
    [(v2 a) (cons (+ a 1) '())]
    [(v2 a b c ...) (cons (+ a 1) (v3 b c ...))]))

(v3 1 2 3 4 5 6 7 8)

;; with any lambda instead of just +1

(define-syntax v4
  (syntax-rules ()
    [(v2 proc a) (cons (proc a) '())]
    [(v2 proc a b c ...) (cons (proc a) (v4 proc b c ...))]))

(v4 (lambda (x) (+ 1 x)) 1 2 3 4 5 6 7 8)

;; two lists

(define-syntax (v5 stx)
  (syntax-case stx ()
    [(_ (a) (b)) #'(cons (list (+ 1 a) (+ 1 b)) '())]
    [(_ (a1 a2 ...) (b1 b2 ...)) #'(cons (list (+ 1 a1) (+ 1 b1)) (v5 (a2 ...) (b2 ...)))]))

(v5 (1 2 3) (4 5 6))
