#lang pie

(claim one Nat)
(define one (add1 zero))

(claim two Nat)
(define two (add1 one))

(claim vegetables (Pair Atom Atom))
(define vegetables (cons 'celery 'carrot))

(which-Nat 5 0 (λ (n) (+ 6 n)))
