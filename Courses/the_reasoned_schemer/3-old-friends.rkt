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

(defrel (listo l) (conde ((nullo l)) ((fresh (d) (cdro l d) (listo d)))))

;; lets try

(run* x (listo `(a b ,x d)))

(run 5 x (listo `(a b c . ,x)))

;; define list of lists

(defrel (lolo l)
        (conde ((nullo l) succeed)
               ((fresh (a) (caro l a) (listo a)) (fresh (d) (cdro l d) (lolo d)))
               (succeed fail)))

(run* q (fresh (x y) (lolo `((a b) (,x c) (d ,y)))))

;; list of singletons

(defrel (singletono l) (fresh (a) (== `(,a) l)))

(defrel (loso l)
        (conde ((nullo l)) ((fresh (a) (caro l a) (singletono a)) (fresh (d) (cdro l d) (loso d)))))

(run 2 z (loso `((a) (b)))) ;; => (_0)

(run 1 z (loso `((g) . ,z)))
;;=> ()
;; The list will become LOS (list of singletons) if z is empty
