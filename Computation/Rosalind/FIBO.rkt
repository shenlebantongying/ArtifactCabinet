#lang typed/racket
(require math/matrix)

(define d
  (assert (string->number (string-trim (port->string (open-input-file "./data/rosalind_fibo.txt"))))
          exact-integer?))

(define step (matrix [[0 1] [1 1]]))
(define init (matrix [[0 1]]))

(: rec (-> Integer (Matrix Integer)))
(define (rec n)
  (if (equal? n 1)
      init
      (matrix* (rec (- n 1)) step)))

(rec d)
