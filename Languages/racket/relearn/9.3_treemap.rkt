#lang racket
(require rackunit)

(define (llt-map fun lst)
  (cond
    [(empty? lst) empty]
    [(list? (car lst)) (cons (llt-map fun (car lst)) (llt-map fun (cdr lst)))]
    [else (cons (fun (car lst)) (llt-map fun (cdr lst)))]))

(check-equal? (llt-map add1 (list 2 (list 3 (list 5)))) (list 3 (list 4 (list 6))))
(check-equal? (llt-map (lambda (x) (string-append x "!")) (list "hi" (list "how" "r") "u?"))
              (list "hi!" (list "how!" "r!") "u?!"))
(check-equal? (llt-map (lambda (n) (list->string (build-list n (lambda (i) #\.))))
                       (list (list 2 3) (list 4 (list 5))))
              (list (list ".." "...") (list "...." (list "....."))))
