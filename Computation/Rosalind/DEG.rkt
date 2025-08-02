#lang racket

;; Note that 1st line is n,m -> vertices & edges
(begin
  (define seq
    (rest (map (lambda (s) (map string->number (string-split s)))
               (string-split (port->string (open-input-file "./data/rosalind_deg.txt")) "\n")))))

(define vertices (sort (remove-duplicates (flatten seq)) <))

(display (string-join (map (lambda (v)
                             (number->string (length (filter (lambda (pair) (member v pair)) seq))))
                           vertices)
                      " "))
