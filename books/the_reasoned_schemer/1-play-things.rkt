#lang racket

(require "trs2-impl.rkt")
(require "trs2-arith.rkt")

(run* q (fresh (x) (== 'pea x)))

(run* q (== `(((pea)) pod) `(((pea)) ,q)))
;; q -> pea

(run* q
  (fresh (x)
    (== `(((,q)) ,x) `(((,x)) pod))))
; q -> pod
;; Why? ,q and ,x are associated, and ,x is associated with pod.

(run* q
  (fresh (x)
    (== `(,x ,x) q))) ;; '((_0 _0))

(run* q
  (fresh (x)
    (== `(,q 'pod) `(,x ,x)))) ;; (pod)

;; conj => and

(run* q
  (conj2 succeed (== 'corn q))) ;; (corn)

;; disj => or

(run* q
  (disj2 succeed fail)) ;; => '(_0)

(run* q
  (disj2 fail fail)) ;; => '()

(run* q
  (disj2 (== 'a q) (== 'b q))) ;; '(a b)

;; defrel

(defrel (teacupo t)
  (disj2 (== 'tea t) (== 'cup t)))

(run* x
  (teacupo x))

(run* (x y)
  (disj2
   (conj2 (== #f x) (== #t y))
   (conj2 (teacupo x) (== #f y))))

(run* (x y)
  (conde
   ((== #f x) (teacupo y))
   ((teacupo x) (teacupo x))))
;; '((#f tea) (#f cup) (tea _0) (cup _0))

(run* (x y)
  (conde
   ((== 'red x) (== 'blue y))
   ((== 'white x) (== 'dark y))))
;;'((red blue) (white dark))
