#lang racket

;; The little learner

(require malt/base-no-duals)
(require (prefix-in plot: plot))

(define (line x)
  (lambda (theta)
    (+ (* (tref theta 0) x) (tref theta 1))))

((line (tensor 1 2)) (tensor 1 1))

(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

(define l2-loss
  (λ (target)
    (λ (xs ys)
      (λ (theta)
        (let ([pred-ys ((target xs) theta)])
          (sum (sqrt (- ys pred-ys))))))))

(((l2-loss line) line-xs line-ys) (tensor 0.0 0.0))

(plot:plot
 (plot:points
  (map vector line-xs line-ys)
  #:x-min 0 #:x-max 5
  #:y-min 0 #:y-max 5))

;;
