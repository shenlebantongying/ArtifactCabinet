#lang racket
(require racket/trace)

;; classify a list into a pair of two lists based on a judgement

(define (classify judge lst)
  (cond
    [(empty? lst) (list empty empty)] ;; termination
    [else
     (local [(define lfd (classify judge (cdr lst)))] ;; lfd => list from deeper
       (cond
         [(judge (first lst))
          (list (cons (first lst) (first lfd))
                (second lfd))]
         [else
          (list (first lfd)
                (cons (first lst) (second lfd)))]))]))

(trace classify)

;; classify based on if odd or not
(classify odd? (list 5 4 3 2 1))
;; => '((5 3 1) (4 2))

;;classify based on if a string has prefix "a"
(classify (λ (s) (string-prefix? s "a"))
            (list "alex" "oliva" "alice" "sophia" "athena"))
;; => '(("alex" "alice" "athena") ("oliva" "sophia"))
