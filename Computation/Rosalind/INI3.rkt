#lang racket

(begin
  (define f (open-input-file "./data/rosalind_ini3.txt"))
  (define str (read-line f))
  (define seq (map string->number (string-split (read-line f)))))

(letrec ([group-by-two (lambda (l)
                         (match l
                           [(list) (list)]
                           [(list a b c ...) (cons (list a b) (group-by-two c))]))])

  (display (string-join (map (lambda (pos)
                               (match pos
                                 [(list a b) (substring str a (+ 1 b))]))
                             (group-by-two seq))
                        " ")))
