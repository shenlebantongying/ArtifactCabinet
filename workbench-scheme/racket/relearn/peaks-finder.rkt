;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname peaks-finder) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (reduce x lst)
  (cond
    [(> x (first lst)) (cons x lst)]
    [else lst]))

(define (expand lst)
  (cond
    [(= (length lst) 1) lst]
    [else (reduce (first lst) (expand (rest lst)))]))

(expand (cons 1 (cons 7 (cons 3 (cons 6 (cons 4 (cons 5 '())))))))

;; '(1 7 3 6 4 5) => '(7 6 5)

;; Steps:
;;    (Expand '(1 7 3 6 4 5))
;; => (Reduce 1 (Expand 7 3 6 4 5)
;; => (Reduce 1 (Reduce 7 (Expand 3 6 4 5)))
;; ...
;; => (Reduce 1 (Reduce 7 (Reduce 3 (Reduce 6 (Reduce 4 (5)))))))
;;                                            ^------------^ first reduce
;;                                             ↓
;; => (Reduce 1 (Reduce 7 (Reduce 3 (Reduce 6 (5)))))
;;                                  ^------------^ reduce more
;; => (Reduce 1 (Reduce 7 (Reduce 3 (6 5))))
;;                        ^--------------^
;; => (Reduce 1 (Reduce 7 (6 5))))))
;; => (Reduce 1 (7 6 5))
;; => (7 6 5)

;; Notes:
;;  Inspecting each element from Right to Left.
;;  If a peak is met from the right side then the next peak on left must be bigger.
;;  Thus comparing an element with "the first of existing peaks" will tell if it is also a perk
;;  ** the final result must be a decreasing list.
