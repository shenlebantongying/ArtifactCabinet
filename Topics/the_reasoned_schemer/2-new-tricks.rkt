#lang racket

(require "trs2-impl.rkt")
(require "trs2-arith.rkt")

(run* q (caro '(a c o r n) q))

;; normal scheme
(cons (car '(grape raiin pear)) (car '((a) (b) (c))))

;; kanren!
(run* r (fresh (x y) (caro '(grape raiin pear) x) (caro '((a) (b) (c)) y) (== (cons x y) r)))
;; r => '((grape a))

(run* l (fresh (x) (cdro l '(c o r n)) (caro l x) (== 'a x)))
;; l => '((a c o r n))
; Note that
; the cdr of l is '(c o r n)
; the car of l is x -> 'a
; thus folling the constrains -> l would be acorn

(run* x (conso x '(a b c) '(myth a b c)))
;; x => myth
;; because only when x->myth, (cons x '(a b c)) will equal to '(myth a b c)
