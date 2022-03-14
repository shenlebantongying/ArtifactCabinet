#lang racket

;; fanncier sum
(define-syntax (complex-sum stx)
  (datum->syntax stx (apply + (cdr (syntax->datum stx)))))

(complex-sum 1 2 3)

;; hold form of to-be-evaluated args
(define (display-and-return x)
    (displayln x)
    x)

(define-syntax (hold-form-if stx)
  (define syx-list (syntax->list stx))
  (displayln syx-list)
  (datum->syntax stx `(cond [,(cadr syx-list) ,(caddr syx-list)]
                            [else ,(cadddr syx-list)])))

(hold-form-if #t
              (display-and-return "true")
              (display-and-return "false"))

