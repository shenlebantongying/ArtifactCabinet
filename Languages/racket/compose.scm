;; combinators

(define (m-compose f g)
  (lambda args
    (f (apply g args))))

((m-compose (lambda (a) (+ 1 a)) (lambda (a b c) (* a b c))) 1 2 3)

(define (m-compose-multi-values f g)
  (lambda args
    (call-with-values (lambda () (apply g args)) f)))

(define tri-passer (m-compose-multi-values
  (lambda (a b c) (+ a b c))
  (lambda (a b c) (values (+ 1 a) (+ 1 b) (+ 1 c)))))

(tri-passer 1 2 3)

;; define nested curried functions
(define ((ok a) b) (cons a b))
((ok 1) 2)


(define ((iter n) f)
  (if (= n 0)
      identity
      (m-compose f ((iter (- n 1)) f))))

(((iter 10) (lambda (a) (+ 1 a))) 1)

