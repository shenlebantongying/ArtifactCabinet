#lang racket

(define as
  (for/list ([l (in-lines (open-input-file "./table_monoisotopic_mass.txt"))])
    (apply (lambda (tag weight) (list (first (string->list tag)) (string->number weight)))
           (string-split l))))

(apply +
       (map (lambda (c) (second (assv c as)))
            (string->list (read-line (open-input-file "./data/rosalind_prtm.txt")))))
