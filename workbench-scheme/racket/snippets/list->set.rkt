#lang racket
(define (list->set a)
  (cond
    [(empty? a) (list)]          
    [(member (first a) (rest a))
        (list->set (rest a))]
    [else
        (cons (first a) (list->set (rest a)))]))
                                                                                                                                                                                                                                                                                                   

(list->set (cons 2 (cons 4 (cons 6 (cons 8 (cons 2 (cons 4 empty)))))))
