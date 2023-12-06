#lang racket

;; f will be a function that will, in future, take a continuation.

;; If we uses the continuation inside f, the control will returns to f,
;; instead of executing the next inloop

;; k is the end of (repeat

(define (repeat v f)
  (call/cc
   (lambda (k)
     (letrec ([inloop (lambda (cur)
                        (f k cur)
                        (inloop (- cur 1)))])
       (inloop v)))))

;; without a call to exit-cc, this will run forever
(repeat 7 (lambda (exit-cc v)
            (if (> v 0)
                (displayln v)
                (begin (exit-cc)
                       (displayln "Will this be reached? No.")))))