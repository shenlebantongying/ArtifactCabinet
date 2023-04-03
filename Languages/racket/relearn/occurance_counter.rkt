#lang racket

(define (ct lst counter)
  (cond    
    [(= (length lst) 1)
     (cond [(equal? (first lst) 'B) (list (add1 counter))]
           [(equal? (first lst) '-) (if (= 0 counter)
                                        (list)
                                        (list counter))])]
    [(and (equal? (first lst) 'B)
          (equal? (second lst) '-)) (cons (add1 counter) (ct (rest lst) 0))]
    [(and (equal? (first lst) 'B)
          (equal? (second lst) 'B)) (ct (rest lst) (add1 counter))]
    [(equal? (first lst) '-) (ct (rest lst) 0)]))


(ct '(B B - - B - B B B -) 0)
(ct '(B B - - B - B B) 0)
(ct '(- B B B - - B - B -) 0)