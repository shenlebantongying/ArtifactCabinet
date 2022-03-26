#lang racket

;; CC -> describe the rest of the computation
(+ (call/cc
    (lambda (k^) ;; k is an escape precedure
      (/ (k^ 5) 4))) 8)

;; k^ = (lambda^ (v) (+ v 8))

;; cc as first-class objects


(define +8^ null)

(+ (call/cc
    (lambda (k^)
      (begin
        (set! +8^ k^)
        (display "inside ->")
        5)))
 8)

(+ 1 (/ (+8^ 10) 0)) ;; => always 18, note that the outer calculations doesn't matter
