#lang racket

;; date: Saturday, February 12, 2022
;; TODO: implement this in a functional style?
;; TODO: try exercies

(define X '(A B C B D A B))
(define Y '(B D C A B A))
(define m (add1 (length X)))
(define n (add1 (length Y)))

(define c (make-list m (make-list n 0)))
(define b (make-list m (make-list n 0)))

(define (2d-set ary x y v)
  (list-set ary x (list-set (list-ref ary x) y v)))

(define (2d-get ary x y)
  (list-ref (list-ref ary x) y))

(define (2d-print ary)
  (for-each displayln ary))

(for ([i (range 1 m)])
  (for ([j (range 1 n)])
    (cond
      [(equal? (list-ref X (sub1 i)) (list-ref Y (sub1 j)))
         (set! c (2d-set c i j (add1 (2d-get c (sub1 i) (sub1 j)))))
         (set! b (2d-set b i j '↖))]
      [(>= (2d-get c (sub1 i) j) (2d-get c i (sub1 j)))
         (set! c (2d-set c i j (2d-get c (sub1 i) j)))
         (set! b (2d-set b i j '↑))]
      [else
         (set! c (2d-set c i j (2d-get c i (sub1 j))))
         (set! b (2d-set b i j '←))])))

(2d-print b)
(displayln "")
(2d-print c)

;; TODO: use a better recrusion
(define (print-lcs i j)
  (if (or (= i 0) (= j 0))
      (list)
      (begin (displayln (list i j))
             (match (2d-get b i j)
               ['↖ (append (print-lcs (sub1 i) (sub1 j)) (list (list-ref X (sub1 i))))]
               ['↑ (print-lcs (sub1 i) j)]
               ['← (print-lcs i (sub1 j))]))))


(print-lcs (length X) (length Y))

;; Solution is '(B C B A) as told by the book

;'(A B   C   B D A B))
;'(  B D C A B   A))
;    ^   ^   ^   ^

;; However, there is another solution -> '(B D A B)

;'(A B C B D   A B))
;'(  B     D C A B A))
;    ^     ^   ^ ^
