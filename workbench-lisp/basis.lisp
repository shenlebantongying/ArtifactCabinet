;; special forms

;; labels -> local function binding
;; (note flet vs labels)
(labels ((f1 (x) (* x x))
         (f2 (x) (+ x x)))
    (f1 3))

(let ((af (lambda (x) (+ x x)))) 
    (funcall af 2))

;; reduce
(reduce (lambda (x y) (+ x y)) '(1 2 3) :initial-value 10)

(reduce #'* '(1 2 3 4 5))