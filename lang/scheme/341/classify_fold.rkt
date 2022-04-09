#lang racket
(require racket/trace)

;; helper: a special (second lst)
;;  when the lst is empty just return empty
(define (second* lst)
  (if (empty? lst) empty (second lst)))

(define (classify fun x lfd) ;; lfd => list from deeper
     (cond
         [(fun x)
          (list (cons x (first lfd))
                (second* lfd))]
         [else
          (list (first lfd)
                (cons x (second* lfd)))]))

;; "fuse" odd? into our classify
(define (classify-odd? x lfd)
  (classify odd? x lfd))

(trace classify-odd?)

(foldr classify-odd? (list empty empty) (list 7 6 5 4 3 2 1))

;;classify based on if a string has prefix "a"
(foldr (λ (s lfd)
         (classify  (λ (s) (string-prefix? s "a")) s lfd))
       (list '() '())
       (list "alex" "oliva" "alice" "sophia" "athena"))
