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

(reduce #'* '(1 2 3 4 5))i

;; mini data as code
(defvar x '(+ 1 2))
(eval x)

;;;;;; [Types]

;; String

(defvar s1 "cat")
(defvar s2 "dog")
(concatenate 'string s1 " " s2)
;;           ^ a string is _specialized vector_

(search "at" s1) ;; => 1

;; Hash table

(defvar h (make-hash-table :test #'equal))
;;                         ^ TODO: if hash is pure int type, this can be omitted
(setf (gethash 1 h) 2)
(setf (gethash "animal" h) "cat")
(gethash 1 h)
(gethash "animal" h)
(gethash "whatever" h)
