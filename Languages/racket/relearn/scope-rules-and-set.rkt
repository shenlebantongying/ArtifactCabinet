#lang racket

(define x 7)
(define f1
  (lambda ()
    (begin
      (set! x (+ x 1))
      (+ x 1))))
(define f2
  (lambda (x)
    (begin
      (set! x (+ x 1))
      (+ x 1))))
(define f3
  (let ([x 5])
    (lambda ()
      (begin
        (set! x (+ x 1))
        (+ x 1)))))
(define f4
  (lambda ()
    (let ([x 7])
      (begin
        (set! x (+ x 1))
        (+ x 1)))))

(define a1 (f1)) ;; 9 -> global x mutate to 8 and return (+ x 1)
(define a2 (f2 x)) ;; 10 -> global x doesn't change, an inner x was changed
(define a3 (f3))
(define a4 (f4))

(define a5 (f1))
(define a6 (f2 x))
(define a7 (f3))
(define a8 (f4))

(values a1 a2 a3 a4)

(displayln "")

(values a5 a6 a7 a8)
