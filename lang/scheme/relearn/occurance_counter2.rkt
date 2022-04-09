#lang racket

;; Count how many B since beginning
;; (count-B '(B B B - -)) => 3
(define (count-B lst)
  (if (empty? lst)
      0
      (if (equal? (first lst) 'B)
          (add1 (count-B (rest lst)))
          0)))

;; Cut first N from the list
;; (list-cut 3 '(1 2 3 4)) => '(4)
(define (list-cut N lst)
  (if (= N 0)
      lst
      (list-cut (sub1 N) (rest lst))))

(define (ct lst)
  (cond [(empty? lst) empty]
        [(equal? (first lst) '-) (ct (rest lst))] ;; If encounter '-, skip it
        [(cons            ;; Combine
          (count-B lst)   ;; The number of following B
          ;; and reapply (ct lst) to the lst with first a few B chopped
          (ct (list-cut (count-B lst) lst)))])) 

(ct '(B B - - B - B B B B B - B -))
(ct '(- B - - B - B B B B B - B B))