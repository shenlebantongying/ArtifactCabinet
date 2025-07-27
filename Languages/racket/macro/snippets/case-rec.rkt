#lang racket

#|
Recrusive case
https://docs.racket-lang.org/reference/case.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._case%29%29

(case val-expr case-clause ...)
  case-clause = | [(datum ...) then-body ...+]
                | [else then-body ...+]
|#

;; Note: this uses recrusive to eval each clause

;; Note: this uses a helper to avoid eval val-expr multiple times

(define-syntax-rule (mycase val clause ...)
  (let ([val-evaled val]) (mycase-aux val-evaled clause ...)))

(define-syntax mycase-aux
  (syntax-rules ()
    [[_ val] (void)]
    [[_ val (else then-body ...)]
     (begin
       then-body ...)]
    [[_ val [(datum ...) then-body] clause ...]
     (if (member val '(datum ...))
         then-body
         (mycase-aux val clause ...))]))

(mycase 5 ([1 2] #t) ([3 4] ((lambda () #f))))

(mycase 7 ([1 2] #t) ([3 4] #f) (else 'sth))
