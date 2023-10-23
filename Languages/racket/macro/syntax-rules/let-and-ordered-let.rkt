#lang racket

;; classic (let

(define-syntax mlet
  (syntax-rules ()
    [(_ ([x e] ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

(mlet ([x 1] [y 2] [op +]) (op x y))

;; racket-y equivalent

(define-syntax-rule
  (mlet2 ([x e] ...) b1 b2 ...)
  ((lambda (x ...) b1 b2 ...) e ...))

(mlet2 ([x 1] [y 2] [op +]) (op x y))

;; let but bindings are evaled in order
;; aka (let*

(define-syntax mlet3
  (syntax-rules ()
    [(_ () b) b]
    [(_ () b1 b2 ...) (b1 (mlet3 () b2 ...))]
    [(_ ([x1 e1] [x2 e2]...) b1 b2 ...) 
     ((lambda (x1) (mlet3 ([x2 e2] ...) b1 b2 ...)) e1)]))

(mlet3 ([x 1]
        [y (+ x x)]
        [z (+ x y)]) (list x y z))

;; alternative (let* implementation with (let

(define-syntax mlet4
  (syntax-rules ()
    [(_ () b1 b2 ...) (begin b1 b2 ...)]
    [(_ ([x1 e1] [x2 e2]...) b1 b2 ...) 
     (let [(x1 e1)] (mlet4 ([x2 e2] ...) b1 b2 ...))]))

(mlet4 ([x 1]
        [y (+ x x)]
        [z (+ x y)]) (list x y z))
