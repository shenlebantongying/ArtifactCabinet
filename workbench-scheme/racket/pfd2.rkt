;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pfd2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (aux num acc) 
  (cond
    [(= num acc) (list num)]
    [(zero? (remainder num acc)) (cons acc (aux (/ num acc) 2))]
    [else (aux num (add1 acc))]))

(define (pfd num)
  (aux num 2))

(pfd 2)
(pfd 24)
(pfd 42)
