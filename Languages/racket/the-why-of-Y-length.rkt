#lang racket

;; A Lecture on the Why of Y by Matthias Felleise

(((λ (le)
    ((λ (mk-length) (mk-length mk-length)) (λ (mk-length) (le (λ (x) ((mk-length mk-length) x))))))
  (λ (length)
    (lambda (l)
      (if (empty? l)
          0
          (add1 (length (cdr l)))))))
 (list 1 2 3 4 6 7))

;; generic Y combinator

(define Y
  (lambda (fun)
    ((lambda (f) (fun (lambda (x) ((f f) x)))) (lambda (f) (fun (lambda (x) ((f f) x)))))))

((Y (λ (length)
      (lambda (l)
        (if (empty? l)
            0
            (add1 (length (cdr l)))))))
 (list 1 2 3 4))
