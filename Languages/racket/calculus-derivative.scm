; Symbolic Computation ( Computer Algebra )
; TODO Reviewing `2.3 Symbolic Data` of SICP <https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3>

;You may want to implement in another PL without dynamic typing, like Java or even pure-C.

;; scheme/derivative.scm

; Define `(deriv exp var)` that takes a symbolic expression that contains symbol var, and reduce the orders of var.

;A simple calculator for derivatives like

;;;
;d(x + 1) / dx = 1
;d(* x y) / dx = y
;d( x * y *( x + 3)) / dx = (3+2x)y
;;;

; Technical Notes

;Scheme -> GNU Guile 3.x


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

; -> caddr is the third elemet of '(x y z)
(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
         ; -> if plain number like 1,2,3..., return 0
        ((variable? exp) (if (same-variable? exp var) 1 0))
         ; -> ordinary derivative takes 1 symbol a time
        ((sum? exp)
          ;-> when encounter a sum, make a pair of sum from each elements' deriv
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))

        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)

; x*y * ( x + 3) => x * y + y * (x + 3)
(deriv '(* (* x y) (+ x 3)) 'x)
;=> (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))


;; Note:

;; There is a pattern here:
;; Match the elements with certain operations,
;; One those elements are processed, they need to be recomposed as result.
;; Pattern-matching (Divide) => Process each element (conquer) => Combine results (Recompsing)
