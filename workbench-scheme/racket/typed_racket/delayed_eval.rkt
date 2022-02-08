#lang typed/racket

(provide Promiseof delay force)

(define-type (Promiseof A)
  (Boxof (U (-> (Boxof A)) (Boxof A))))

(define-syntax-rule (delay e) (box (λ () (box e))))

(: force : (All (A) ((Promiseof A) -> A)))
(define (force b)
  (let ([ub (unbox b)])
    (if (procedure? ub)
        (let ([v (ub)])
          (set-box! b v)
          (unbox v))
        (unbox ub))))

(force
 (ann (delay (+ 1 2))
      (Promiseof Integer))) ;; ensure type checking

;; todo -> read https://docs.racket-lang.org/ts-guide/caveats.html#%28part._.Type_inference_for_polymorphic_functions%29