;; stream pattern

#lang racket

;; minimum version, will the cdr of list will always be 1
(define ones (lambda () (cons 1 ones)))

;; => a data structure that represent 1 to asdsd
(define powers-of-two
    (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
        (lambda () (f 2))))

;;;; generalize stream pattern

(define (stream-maker fn arg)
    (letrec ([aux (lambda (x)
                    (cons x (lambda () (aux (fn x arg)))))])
        (lambda () (aux arg))))

(define powers-of-three (stream-maker * 3))

(define n powers-of-two)

n
(n)
((cdr (n)))
((cdr ((cdr (n)))))
(car ((cdr ((cdr (n)))))) ;; -> 2^3 = 8

;;;; generalize value retrive function
;; get nth value of the stream

; TODO: can u use something like yield?

(define (retrive stream n)
    (if (= n 1)
        (car (stream))  ; <-------------------------------|
        (retrive (cdr (stream)) (- n 1))))                ;
                                                          ;
(retrive n 3) ;; -> 8                                     ;
(retrive powers-of-two 3)                                 ;
(retrive powers-of-three 3)                               ;
                                                          ;
;; reverse searching                                      ;
;; Keep getting new value, untile a value is found        ;
                                                          ;
(define (number-until stream tester)                      ;
    (letrec ([aux (lambda (stream acc)                    ;
                (let ([val (stream)]); <------------------|
                                     ; eval (steam) one time to make it become a pair
                    (if (tester (car val))
                    acc
                    (aux (cdr val) (+ acc 1)))))])
    (aux stream 1)))

(number-until powers-of-two (lambda (x) (= x 16))) ; -> 4
