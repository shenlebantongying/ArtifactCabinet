#lang racket

;;
(+ 1 (call/cc (lambda (esc) (esc 2))))

;;
(let ([p1 (call/cc (lambda (k) (k 3)))]) (+ 1 p1))

;;
(let ([p1 (call/cc (lambda (k) (k "with this arg")))])
  (begin
    (display "Called -> ")
    (displayln p1)))

;;
(cons 'a (call/cc (lambda (k) (k (cons 'b '())))))

;;
(let ([list-reverser
       (lambda (lst1)
         (let ([rev (call/cc (lambda (doSth)
                               (letrec ([mrec (lambda (l acc)
                                                (cond
                                                  [(empty? l) (doSth acc)]
                                                  [else (mrec (cdr l) (cons (car l) acc))]))])
                                 (mrec lst1 '()))))])
           (begin
             ;; same continuation used in different contexts
             (writeln (cons 99 rev))
             (writeln rev))))])
  (list-reverser '(1 2 3)))

;; take a continuation
(define (apply-cc cc)
  (if (continuation? cc)
      (apply + cc)
      (displayln "Not continuation!")))

(apply-cc (call/cc (lambda (s) (s (list 1 2 3)))))

;;
(define (pass-to-taker f)
  (+ 4
     (call/cc (lambda (cc)
                ;; Note how we pass the current continuation, aka (+ 4 []), to outer world
                (f cc)))))

(pass-to-taker (lambda (cc-intake) (cc-intake 2)))
