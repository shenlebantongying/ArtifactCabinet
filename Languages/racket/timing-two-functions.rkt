#lang racket

(define target-length #e5e4)

(define (A current-list new-element)
  (append current-list (list new-element)))

(define (B current-list new-element)
  (reverse (cons new-element (reverse current-list))))

(define (profile function)
  (printf "~a: " (object-name function))
  (time (void (for/fold ([l null]) ([n (in-range target-length)])
                (function l n)))))

(profile A)
(profile B)
