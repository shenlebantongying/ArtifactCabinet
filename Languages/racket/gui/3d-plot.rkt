#lang racket
(require racket/gui/base
         plot)

(define n 2) ;; mutable

(define (the-graph-plot-function dc)
  (plot3d/dc
   (surface3d (λ (x y) (sin (+ x (expt y n)))) -5.1 5.1 -2.1 2.1 #:samples 60 #:label "z = x + y^n")
   dc
   0
   0
   600
   600))

(define main-frame (new frame% [label "A 3d wave plot"] [width 600] [height 600]))

(define canvas
  (new canvas%
       [parent main-frame]
       [paint-callback
        (λ (canvas dc)
          (send dc set-smoothing 'smoothed)
          (the-graph-plot-function dc))]))

(define slider
  (new slider%
       [label "Value of n"]
       [parent main-frame]
       [min-value 0]
       [max-value 4]
       [init-value 2]
       [callback
        (λ (islider ev)
          (set! n (send islider get-value))
          (send canvas refresh-now))]))

(send main-frame show #t)
