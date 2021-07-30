#lang racket

;; Bookmark: Higher-order list operations

(displayln "generic recursion map")
(define (map/slb f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (map/slb f (cdr lst)))))

(map/slb (λ (x) (+ x 1)) '(1 2 3))

(displayln " generic map with match")
;; this one is very similar to 
(define (map/match f lst)
  (match lst
    ['() '()]
    [(cons hd tl) (cons (f hd) (map/match f tl))]))

(map/match (λ (x) (- x 1)) '(0 1 2))

(displayln "map via list Comprehensions")
;; TODO: more examples?
;; How to do this in haskell?
(for/list ([x '(1 2 3)]) (+ x 1))

(displayln "filtering in recursion")
(define (filter/slb p? lst)
  (cond
    [(null? lst) '()]
    [(p? (car lst)) (cons (car lst)
                          (filter/slb p? (cdr lst)))]
    [else (filter/slb p? (cdr lst))]))

(define (even/slb? n)
  (= 0 (modulo n 2))
)

(filter/slb even/slb? '(1 2 3 4))

(displayln "filter in pattern matching")
(define (filter/match p? lst)
  (match lst
    ['() '()]
    ;; (? expr pat ...) match if (expr value) and pats
    [(cons (? p?) tl) (cons (car lst) (filter/match p? (cdr lst)))]
    [(cons hd tl) (filter/match p? tl)]))

(filter/match even/slb? '(-2 -1 0 1 2))

(displayln "filter via list comprehension")
;; add 1 for all odds
(for/list ([x '(1 2 3 4)] #:when (odd? x)) (+ x 1))

(displayln "abstract map more")
(define (abstract-map kons nil f lst)
  (if (null? lst)
      nil
      (kons (f (car lst))
            (abstract-map kons nil f (cdr lst)))))
;; sum, by providing "concating function -> kons" as +
;;                   init value -> nil condition as 0
(abstract-map + 0 identity '(1 2 3 4))

;; add1 again
(abstract-map cons '() (λ (x) (+ x 1)) '(1 2 3))

;; sum of all squares
(abstract-map cons '() (λ (x) (* x x)) '(3 4))
(abstract-map +    0   (λ (x) (* x x)) '(3 4))


(displayln "Zipping")
(for/list ([x '(1 2 3 4 5)]
           [y '(4 5 6 7 8)])
  (list x y))

