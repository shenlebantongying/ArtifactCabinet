#lang racket

;; "1" ->
;; "1+1+1" -> "1" "+" "1+1"

(define (op? x)
  (or (equal? x #\+)
      (equal? x #\-)
      (equal? x #\*)
      (equal? x #\/)))

(define (parse s pos)
  (cond
    [(or (= 0 (string-length s))
         (= pos (string-length s))) (substring s 0 pos)]
    [else (let ([h (string-ref s pos)])
            (cond [(op? h) (list (substring s 0 pos) h (parse (substring s (add1 pos)) 0))]
                  [(char-numeric? h) (parse s (+ pos 1))]))]))

(parse "1+1" 0)

