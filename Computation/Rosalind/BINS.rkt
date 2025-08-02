#lang racket

(define data (string-split (port->string (open-input-file "./data/rosalind_bins.txt")) "\n"))

(define dataseq (map string->number (string-split (third data))))
(define targets (map string->number (string-split (fourth data))))

(define (bsearch seq target)
  (letrec ([rec (lambda (t left right)
                  (let* ([mid (quotient (+ left right) 2)]
                         [n-in-mid (list-ref seq mid)])
                    (cond
                      [(equal? n-in-mid t) (+ 1 mid)]
                      [(or (equal? mid left) (equal? mid right)) -1]
                      [(> n-in-mid t) (rec t left mid)]
                      [(< n-in-mid t) (rec t mid right)])))])
    (rec target 0 (length seq))))

(for-each (curry printf "~a ") (map (curry bsearch dataseq) targets))
