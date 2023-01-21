#lang racket
(require racket/engine)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define eng
  (engine
   (lambda (x) (fib 30))))

;; Engine can be used to limiting run time

;; Code below can measure the time of execuation
;; (which is bad)

(define (mileage)
    (if (engine-run 10 eng)
        10
        (+ (mileage) 10)))

(mileage)

(current-logger)