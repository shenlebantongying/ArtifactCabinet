#lang racket

;; What the hell is this?

;; This creates a precedure that is internally recursive but not externally named.

;; (lambda is anonymous, but this is self-refering

;; Also see srfi-31

(define-syntax rec
  (syntax-rules ()
    [(_ (name . variables) . body)
     (letrec ([name (lambda variables
                      . body)])
       name)]
    [(_ name expr)
     (letrec ([name expr])
       name)]))

((rec (fib n)
      (if (zero? n)
          1
          (* n (fib (- n 1)))))
 5)

(map (rec sum
          (lambda (x)
            (if (= x 0)
                0
                (+ x (sum (- x 1))))))
     '(1 2 3 4))

;; TODO complex examples https://scheme.com/tspl4/syntax.html#./syntax:h0
