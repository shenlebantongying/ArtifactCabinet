;; chicken scheme works better than mit
;; https://r7rs.org/large/fascicles/macro/1/other-macro-systems.html#explicit-renaming-macros

(define-syntax fancy-pair!
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((x (cadr form))
           (y (caddr form)))
       `(list ,x ,y)))))

(fancy-pair! 1 2)

(expand '(fancy-pair! 1 2))

(define-syntax rename-no-reason!
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((head (cadr form)))
       `(,(rename 'hello))))))

(expand '(rename-no-reason! hello))

(define-syntax compare-no-reason!
  (er-macro-transformer
   (lambda (form rename compare)
     (let* ((h (cadr form))
            (h1 (rename h)))
       `(,compare ,h ,h1)))))

(expand '(compare-no-reason! pp))

(compare-no-reason! 'hello)
