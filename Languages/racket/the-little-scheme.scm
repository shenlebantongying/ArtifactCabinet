;; All code & experiments of
;; "The Seasoned Schemer"
;;
;; Programmed on Chez

;; $8 -> Lambda the Ultimate

(define rember-f;irst
  (lambda (test? a l)
    (cond
     ((null? l) (quote ()))
     ((test? (car l) a) (cdr l))
     (else (cons (car l)
                 (rember-f  test? a (cdr l)))))))
(rember-f eq? 'a '(a b c a d e c f))

;; Currying
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

; We can craft a "sub-type" of the function like inhertance?
(define eq?-salad (eq?-c 'salad))
((eq?-c 'a) 'a)
(eq?-salad 'salad)

;; Use the idea again
(define rember-f2
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) a) (cdr l))
       (else (cons (car l)
                   ((rember-f2 test?) a (cdr l))))))))
((rember-f2 eq?) 'a '(b a c d a b a))

; TODO: here

;; $9 ->  ... and Again, and Again, and Again, ...

;; $11 -> Welcome back to the show

(define is-first?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (eq? a (car lat))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (or (is-first? (car lat) (cdr lat))
          (two-in-a-row? (cdr lat)))))))

;; Leave the decision of wheter continuing the searched
;; to the is-first?

;; Note those two func are defined against each other, and it doesn't matter

;; TODO: can u do this in ML?
;; Yes? Because they has local bindings?

(define is-first-2?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (two-in-a-row-2? lat))))))

(define two-in-a-row-2?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (is-first-2? (car lat) (cdr lat))))))
(two-in-a-row-2? '(1 1 3))

(eq? (two-in-a-row-2? '(cat cat dog)) (two-in-a-row? '(nosure good good)))
