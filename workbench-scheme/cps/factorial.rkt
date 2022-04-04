#lang racket

;; Noraml
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Wizard
(define (fact n k)
  (if (= n 0)
      (k 1)
      (fact (- n 1)
            (λ (x) (k (* n x))))))

(equal? (fact 10 (λ (x) x)) (factorial 10))

;; craft a new continuation, based on the previous one, and pass that to our
;; recursive call

(fact 10 (λ (x) x))

;; eval steps:
(fact 3 (λ (x) x))

;; => (fact 2 (λ (x) ((λ (x) x) (* 3 x)))
;; -> (fact 2 (λ (x) (* 3 x)))

;; => (fact 1 (λ (x) ((λ (x) (* 3 x)) (* 2 x)))
;; -> (fact 1 (λ (x) (* 3 (* 2 x))))

;; -> (* 3 (* 2 1))
;; -> 6
