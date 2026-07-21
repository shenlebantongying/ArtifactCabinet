#lang racket
(require (for-syntax syntax/parse))

;; mini calc with define & primitives

(define-syntax (flow stx)
  (syntax-parse stx
    [(_ ((~datum zero))) #'0]
    [(_ ((~datum define) name val cond ...)) #'(let ([name val]) (flow cond ...))]
    [(_ ((~datum plus) val cond ...)) #'(+ (flow cond ...) val)]
    [(_ ((~datum inc) cond ...)) #'(+ (flow cond ...) 1)]
    [(_ ((~datum dec) cond ...)) #'(- (flow cond ...) 1)]
    [(_ ((~datum multiply) val cond ...)) #'(* (flow cond ...) val)]))

(flow (define x
        10
        (plus x (inc (multiply x (inc (inc (zero))))))))
