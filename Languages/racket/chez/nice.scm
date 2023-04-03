 ;; -*- geiser-scheme-implementation: chez -*-

;; use call/cc to break from the recrusion when 0 is found
(define zero-finder
  (lambda (lst)
    (call/cc
     (lambda (break)
       (letrec  ([f (lambda (x)
                     (cond
                      [(null? x) #f]
                      [(= (car x) 0) (break #t)]
                      [else (f (cdr x))]))]) (f lst))))))
(define nice )

(zero-finder '(1 2 3 0 0))
