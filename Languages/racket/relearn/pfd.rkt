;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pfd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (aux y acc)
  (if (zero? (remainder y acc))
      acc
      (aux y (add1 acc))))

(define (obtain-smallest-factor x)
  (aux x 2))

(define (pfd num)
  (cond
    [(= num (obtain-smallest-factor num)) (list num)]
    [else (cons (obtain-smallest-factor num) (pfd (/ num (obtain-smallest-factor num))))]))

(pfd 24)
(pfd 42) ;; (list 2 3 7)
