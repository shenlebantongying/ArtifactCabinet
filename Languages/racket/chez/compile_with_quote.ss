;; unquote inside quasiquote
`(+ 2 ,(* 2 3))
(eval `(+ 2 ,(* 2 3)))

;; Nesting
`(+ 1 ,(list `(+ 2 ,(- 3 3))))

;; splice vs unqote
`(+ ,@(cdr '(1 2)) 3) ;; => (+ 2 3)
`(+ ,(cdr '(1 2)) 3) ;; => (+ (2) 3)

;; => Compile let into lambda
;; from
(define lll '(let ([a 1] [b 2] [c 3]) (+ a b c)))
;; to
;; ((lambda (a b c) (+ a b c)) 1 2 3)

(define let->lambda
  (lambda (e)
    (let ([binds (cadr e)])
      `((lambda (,@(map car binds)) ,@(cddr e))
	,@(map cadr binds)))))

(let->lambda lll)
(eval (let->lambda lll))

;;; TODO more compile with quotes?
