#lang racket/gui

;; !! Spaghetti below is ugly somehow !!

;;  ----- Global Variables -----------------

(define score 0)

(define window-height 300)
(define window-width 500)

(define ball-x 1)
(define ball-y 1)
(define ball-x-direction +)
(define ball-y-direction +)
(define ball-width 20)

(define pad-width 150)
(define pad-height 10)
(define pad-left 0)

(define pad-y (- window-height pad-height))
(define pad-half-width (/ pad-width 2))

;; ----- GUI init ---------------------------

(define main-frame
  (new frame%
       [label "This is a pong game"]))

(define main-panel
  (new vertical-panel%
       [parent main-frame]))

(define top-bar
  (new horizontal-panel%
       [parent main-frame]
       [alignment '(center center)]))

(define score-message
  (new message%
       [parent top-bar]
       [label "Score: 0"]
       [auto-resize #t]))

(define restart-btn
  (new button%
       [parent top-bar]
       [label "start/restart"]
       [callback (λ (btn ev)
                   (set! score 0)
                   (set! ball-x 0)
                   (set! ball-y 0)
                   (send clock start 1))]))

(define pong-canvas%
  (class canvas%
    (define/override (on-subwindow-event rec ev)
      (when (send ev moving?)
        (set! pad-left (- (send ev get-x) pad-half-width))))
    (super-new)))

(define cav
  (new pong-canvas%
       [parent main-panel]
       [min-width window-width]
       [min-height window-height]
       [paint-callback
        (λ (canvas dc)
          (send dc set-pen "black" 0 'transparent)
          (send dc set-brush "black" 'solid)
          (send dc draw-rounded-rectangle ball-x ball-y ball-width ball-width ball-width)
          (send dc draw-rounded-rectangle pad-left pad-y pad-width pad-height pad-height))]))

(send cav set-cursor (make-object cursor% 'size-e/w))

;; ----- Helpers ------------------------------------------------------------------

(define (invert sign)
  (if (equal? sign +) - +))

(define (score-msg n)
  (string-append "Score: " (number->string n)))

(define (in-range? x low high)
  (and (>= x low) (<= x high)))

(define (collide?)
  (and
   (in-range? ball-x pad-left (+ pad-left pad-width))
   (> (+ ball-y ball-width) pad-y)))

;; ----- Ball & Pad moving logic ------------------------------

(define (ball-move dx dy)
  (cond
    [(or (> ball-x window-width) (< ball-x 0))
     (set! ball-x-direction (invert ball-x-direction))]
    [(< ball-y 0)
     (set! ball-y-direction (invert ball-y-direction))]
    [(> ball-y pad-y)
     (send score-message set-label "You lose.")]
    [(collide?)
     (set! score (add1 score))
     (set! ball-y-direction (invert ball-y-direction))
     (send score-message set-label (score-msg score))])
  (set! ball-x (ball-x-direction ball-x dx))
  (set! ball-y (ball-y-direction ball-y dy))
  (send cav refresh-now))

;; ----- Set the looping timer ---------------------------------

(define clock
  (new timer%
       [notify-callback (λ () (ball-move 0.5 0.5))]))

(send main-frame show #t)

