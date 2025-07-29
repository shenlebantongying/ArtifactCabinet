#lang racket

(define (longest x y)
  (if (> (length x) (length y)) x y))

(define (lcs x y)
  (cond
    [(or (empty? x) (empty? y)) empty]
    [(equal? (car x) (car y)) (cons (car x) (lcs (cdr x) (cdr y)))]
    [else (longest (lcs x (cdr y))
                   (lcs (cdr x) y))]))

(define X '(A B C B D   A B))
(define Y '(  B     D C A B A))

(lcs X Y)

;; Same but with pattern matching
(define (lcs2 x y)
  (match (list x y)
    [(list '() _) '()]
    [(list _ '()) '()]
    [(list (cons xh xs) (cons yh ys))
     (if (equal? xh yh)
         (cons xh (lcs2 xs ys))
         (longest (lcs2 x ys) (lcs2 xs y)))]))

(lcs2 X Y)
