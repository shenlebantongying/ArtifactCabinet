#lang racket

;TODO -> A CLOS-like OOP
(define animal-interface (interface () bark))

(define dog% 
    (class* object% 
        (animal-interface)
        (super-new)
        (define/public 
            (bark) (display "Dog bark!\n"))))

(define mydog (new dog%))

; SEND
(send mydog bark)