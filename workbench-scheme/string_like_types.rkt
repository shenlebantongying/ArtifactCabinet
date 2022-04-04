#lang at-exp racket

#|
 https://docs.racket-lang.org/scribble/reader.html
 prefix syntax
|#

(define (foo str)
    (printf "My message-> ~s \n" str))

(define (slb-mul a b)
    (* a b))

@foo{Thi is nice} ; <- passed as string

@slb-mul[2 3] ; => 6

(displayln "[] Keywords")
; keyword
(define (f #:a x #:b y)
    (* x y))
(f #:a 3 #:b 4)

; is equal to

(keyword-apply f '(#:a #:b) '(4 5) empty)

(keyword-apply f
    (list
        (string->keyword "a")
        (string->keyword "b")) 
'(4 5) empty)