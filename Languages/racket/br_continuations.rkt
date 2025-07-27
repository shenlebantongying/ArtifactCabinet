#lang racket

(define saved-c #f)

(+ 1
   (+ 2
      (let/cc here
        (set! saved-c here)
        3)))
;            ^ make external `c` as
;              a bookmark of this point
; -> 6

; equivant
(define saved-d #f)
(+ 1
   (+ 2
      (call/cc (λ (here)
                 (set! saved-d here)
                 3))))

(saved-c 10)
(saved-d 10)
; -> (+ 1 (+ 2 10))
;              ^
; -> 14

(displayln "save-it!")

(define saved-var #f)
(define (save-it!)
  (call-with-composable-continuation (λ (k)
                                       (set! saved-var k)
                                       0)))

(+ 1 (+ 1 (+ 1 (save-it!))))
