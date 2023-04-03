;; Reading notes for "on Lisp"

;; [Functions]

; two namespaces
(defun double-x (x) (* 2 x))
(setq  double-x 2)
(double-x double-x)
(symbol-value 'double-x)
(symbol-function 'double-x)

;prefix calling
(apply #'+ '(1 2))
(funcall #'+ 1 2)
(mapcar #'1+ '(1 2 3 4))

;; [Macros]

(defmacro nilify! (var)
  (list 'setq var nil))
(defvar x 1)
(nilify! x)
x
;;
(defmacro nilify2! (var)
  `(setq ,var nil))
(defvar y 2)
(nilify2! y)
y
;;

(setq b '(1 2 3))

`(A ,b c)
;=> (A (1 2 3) C)
; comma -> insert list

`(A ,@b c)
;=> (A 1 2 3 C)
; comma-at -> insert elements of list

;; TODO define when/while?
