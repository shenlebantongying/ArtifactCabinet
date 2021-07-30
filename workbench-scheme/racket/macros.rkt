#lang racket
(require racket)

;; define syntax
(define-syntax foo
  (lambda (stx)
    (syntax "Yes")))

(define-syntax (foo2 stx)
  (syntax "Yes2"))

(foo2)


(define-syntax (show-stx stx)
  (print stx)
  #'(void))

(show-stx '(+ 1 2)) ;; #<syntax::77:14 (if x (list "true") #f)>


;; [reverse me]
(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(reverse-me "a" "b" "c" values)

;; How?

; 1.) stx->dat convert input list into a normal list
; 2.) modifed this list via functions
; 3.) covert the list back to evaluatable s-expr

;; [pattern based syntax]

(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

;; TODO write a more generic one
(define-syntax rotate
  (syntax-rules ()
    [(rotate a b) (swap a b)]
    [(rotate a b c) (begin
                      (swap a b)
                      (swap b c))]))
(let ([x 1]
      [y 2])
  (swap x y)
  (list x y))

(let ([x 1]
      [y 2]
      [z 3])
  (rotate x y z)
  (list x y z)
)

