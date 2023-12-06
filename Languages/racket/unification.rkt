#lang racket

;; TODO: read more about continuation & cps before this

;; tspl4 unify

;; assume function looks like '(f [args...])
;; this only unifies the args part

;; u occurs in v?
(define (occurs? u v)
  (and (pair? v)
       (let f ([l (cdr v)])
         (and (pair? l)
              (or (eq? u (car l))
                  (occurs? u (car l))
                  (f (cdr l)))))))

;; extent lambda s by replacing its result
(define (sigma u v s)
  (lambda (x)
    (let f ((x (s x)))
      (if (symbol? x)
          (if (eq? x u) v x)
          (cons (car x) (map f (cdr x)))))))


((sigma 'a 'b (lambda (x) x)) (list 'f 'a 'b 'c))

(define try-subst
  (lambda (u v s ks kf)
    (let ((u (s u)))
      (if (not (symbol? u))
          (uni u v s ks kf)
          (let ((v (s v)))
            (cond
              ((eq? u v) (ks s))
              ((occurs? u v) (kf "cycle"))
              (else (ks (sigma u v s)))))))))

(define uni
  (lambda (u v s ks kf)
    (cond
      ((symbol? u) (try-subst u v s ks kf))
      ((symbol? v) (try-subst v u s ks kf))
      ((and (eq? (car u) (car v))
            (= (length u) (length v)))
       (let f ((u (cdr u)) (v (cdr v)) (s s))
         (if (null? u)
             (ks s)
             (uni (car u)
                  (car v)
                  s
                  (lambda (s) (f (cdr u) (cdr v) s))
                  kf))))
      (else (kf "clash")))))

(define unify
  (lambda (u v)
    (uni u
         v
         (lambda (x) x)
         (lambda (s) (s u))
         (lambda (msg) msg))))

(unify 'x 'y)

(unify '(f x) '(f y))

(unify '(f x y) '(g x y))

(unify '(f x (h)) '(f (h) y))
