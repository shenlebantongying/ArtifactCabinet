#lang racket
(require racket
         "slb.rkt")

(provide slb-len
         add-comma)

;; C-x-e -> eval last S-expr

;; String and symbols

(define (add-comma)
(string-join (map (lambda (x)
       (string-append x ", "))
     (list "A" "B" "C"))))

;; Closure 

(define (slb-len lst)
  (define (iter lst len)
    (cond  ;; TODO replace this with patterns?
      [(empty? lst) len]
      [else (iter (rest lst) (+ len 1))]))
  (iter lst 0)
)


;; [Matrix transpose]

(define (transpose m)
  (apply map list m))

(transpose '((1 2 3) (4 5 6)))

; Steps:
;;   (apply map list '((a b) (d e))
;; = (map List '(a b) '(d e))
;; = (list (List 'a 'd) (List 'b e))
;; = '((a d) (b e))

; note that map will combine values form each lists at same positions
(map (lambda (x y) (+ x y)) '(1 2 3) '(4 5 6))
