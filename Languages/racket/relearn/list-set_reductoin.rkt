;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname p1_reductoin) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Check length of list is 1.
(define (single? lst)
  (= (length lst) 1))

;; Expand calculation
(define (list->set a)
  (cond
    [(single? a) a]
    [else (reduce (first a) (list->set (rest a)))]))

(define (reduce x lst)
  (cond
    [(empty? lst) (list x)]
    [(member x lst) (reduce (first lst) (rest lst))]
    [else (cons x (reduce (first lst) (rest lst)))]))

(list->set (cons 2 (cons 4 (cons 6 (cons 8 (cons 2 (cons 4 empty)))))))

;; '(2 4 6 8 2 4) => '(6 8 2 4)

;; Steps:
;;    (list->set (2 4 6 8 2 4)) ; expansion begin
;; => (R 2 (4 6 8 2 4))
;; => (R 2 (R 4 (6 8 2 4))
;; => (R 2 (R 4 (R 6 (8 2 4))
;; => (R 2 (R 4 (R 6 (R 8 (2 4)))
;; => (R 2 (R 4 (R 6 (R 8 (R 2 (4))))))
;;                        ^-------^ first reduction
;; => (R 2 (R 4 (R 6 (R 8 (2 4)))))
;;                   ^---------^ next reduction
;; => (R 2 (R 4 (R 6 (8 2 4))))
;; => (R 2 (R 4 (6 8 2 4)))
;; => (R 2 (6 8 2 4))
;; => (6 8 2 4)
