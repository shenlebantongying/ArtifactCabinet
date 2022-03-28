#lang racket
(require racket/stxparam)

#|
    syntax parameter solved one type of unhygine macro usage
|#

;; ref: https://www.greghendershott.com/fear-of-macros/Syntax_parameters.html

;; -> code that doesn't work
#;
(define-syntax-rule (aif condition true-expr false-expr)
    (let ([it condition])
      (if it
          true-expr
          false-expr)))
#;
(aif #t (displayln it) (void))
;; the define-syntax-rule internally bind condition to `it`,
;; but we cannot use it as "surrounding" code

;; TODO: port this to lisp/defmacro and check if the `it` will be useable


(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside aif")))

(define-syntax-rule (aif condition true-expr false-expr)
  (let ([tmp condition])
    (if tmp
        (syntax-parameterize ([it (make-rename-transformer #'tmp)])
                           true-expr)
        false-expr)))

(aif 10 (displayln it) (void))
;; the it here will be internally
;; transformed to #'tmp which is the contidion we passed
