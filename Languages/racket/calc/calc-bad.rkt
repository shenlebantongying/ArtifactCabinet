#lang racket
(require racket/match)
(require rackunit)

(define (op? x)
  (or (equal? x #\+)
      (equal? x #\-)))

(define (lex s)
  (define (aux s pos)
    (if (or (= 0 (string-length s))
            (= pos (string-length s)))
        (list (string->number (substring s 0 pos)))
        (let ([h (string-ref s pos)])
          (cond [(op? h)
                     (cons (string->number (substring s 0 pos))
                           (cons (string->symbol (string h))
                                 (aux (substring s (add1 pos)) 0)))]
                [(char-numeric? h) (aux s (+ pos 1))]))))
  (aux s 0))

(check-equal? (lex "1+1") '(1 + 1))
(check-equal? (lex "1+1-1") '(1 + 1 - 1))

(define (calc s)
  (cond
    [(= (length s) 1) (first s)]
    [else (match (second s)
            ['+ (+ (first s) (calc (cddr s)))]
            ['- (- (first s) (calc (cddr s)))])]))

(check-equal? (calc (lex "1+1")) 2)
(check-equal? (calc (lex "1+4+2-3")) 4)
