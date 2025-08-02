#lang racket

(define d
  (map string->number (string-split (read-line (open-input-file "./data/rosalind_fibd.txt")))))
(define n (first d))
(define m (second d))

#|

Except the initial sequence, this applies

F(N) = F(N-1) + F(N-2) - F(N-(M+1))

|#

(define (init-seq generation l)
  (let ([f_n1_n2 (+ (first l) (second l))])
    (if (equal? generation m)
        (cons (- f_n1_n2 1) l)
        (init-seq (+ generation 1) (cons f_n1_n2 l)))))

(define (iterate generation l)
  (if (equal? generation (- n 1))
      l
      (iterate (+ 1 generation) (cons (- (+ (first l) (second l)) (list-ref l m)) l))))

(iterate m (init-seq 1 '(1 0)))
