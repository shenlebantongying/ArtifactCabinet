#lang racket

(require racket/future
         future-visualizer)

(define (doSth)
  (begin
    (sleep 3)
    (+ 1 1)))

;; MULTI CORES
(visualize-futures (let ([tasks (for/list ([i (in-range 10)])
                                  (future (lambda () (doSth))))])
                     (for/list ([f (in-list tasks)])
                       (touch f))))
