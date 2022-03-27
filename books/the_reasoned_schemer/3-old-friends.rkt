#lang racket
(require "trs2-impl.rkt")
(require "trs2-arith.rkt")

;; racket
(define (lst? l)
  (cond
    [(empty? l) #t]
    [(cons? l) (lst? (cdr l))]
    [else #f]))

;; kanren

(defrel (listo l)
  (conde
   ((nullo l))
   ((fresh (d)
      (cdro l d)
      (listo d)))))

(run* x
  (listo '(a b c d)))
