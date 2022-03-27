#lang racket

(define-struct node (key left right) #:transparent)

(define (nth lst n)
  (cond
    [(= n 0) (car lst)]
    [else (nth (cdr lst) (sub1 n))]))

(define (after-n lst n)
  (cond
    [(= n 0) (cdr lst)]
    [else (after-n (cdr lst) (sub1 n))]))

(define (before-n lst n)
  (cond
    [(= n 0) empty]
    [else (cons (car lst) (before-n (cdr lst) (sub1 n)))]))


(define (build-balanced-tree lst)
  (local
    [(define n  (sub1 (ceiling (/ (length lst) 2))))]
    (cond
      [(empty? lst) empty]
      [else (make-node (nth lst n)
                       (build-balanced-tree (before-n lst n))
                       (build-balanced-tree (after-n lst n)))])))

(build-balanced-tree (list 1 2 3 4 5 6 7))
