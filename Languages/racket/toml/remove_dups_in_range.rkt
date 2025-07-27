#lang racket

;; for/fold is similar to for-comprehensions in python

(define (remove-duplicates+ lst #:start (start 0) #:end (end #f))
  (for/fold ([rr '()]
             [es (set)]
             #:result (reverse rr))
            ([e (in-list lst)]
             [i (in-naturals)])
    (if (and (>= i start) (or (not end) (< i end)))
        (if (set-member? es e)
            (values rr es)
            (values (cons e rr) (set-add es e)))
        (values (cons e rr) es))))

(define lst '(0 1 1 3 3 5 5))
(remove-duplicates+ lst #:start 1 #:end 6) ;; => '(0 1 3 5 5)
