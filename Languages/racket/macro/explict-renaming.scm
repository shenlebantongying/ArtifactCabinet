;; Racket does not uses this, see mit/scheme's doc
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Macros.html

;; also in
;; chicken -> (import (chicken syntax))

;; it is just a normal transformer if not using rename & compare
;; (call proc arg ...) -> (proc arg ...)
(define-syntax call
  (er-macro-transformer
   (lambda (exp rename compare)
     (cdr exp))))

;;;; porblem: unhygenic if not using rename
(define-syntax a
  (er-macro-transformer
   (lambda (exp rename compare)
     `(cons ,@(cdr exp)))))

;; the outside cons will affect the result!
(let  ((cons +))
  (a 1 2)) ;; -> 3

;;;; hygenic with rename
(define-syntax a2
  (er-macro-transformer
   (lambda (exp rename compare)
     `(,(rename 'cons) ,@(cdr exp)))))

;; werid
;; (expand '(a 1 2))
;; (cons820885 1 2)

(let  ((cons +))
  (a2 1 2)) ;; -> (1 . 2)

;;;; compare

(define-syntax a3
  (er-macro-transformer
   (lambda (exp rename compare)
     (let ((%a (rename 'cons))
           (%b (rename 'cons)))
       (list %a %b (compare %a %b))))))

;; 'cons will be renamed to the same sym
;; (expand '(a3))
;; (cons820956 cons820956 #t)
