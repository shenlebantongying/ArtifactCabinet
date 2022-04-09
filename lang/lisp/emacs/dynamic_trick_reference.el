;; dynamic scoping is generally bad design

;; emacs is dynamic scoping
;; You can modify variables from caller's stack
(defun bar ()
  (setq x 7)) ;; this bar will modify the x inside foo

(defun foo ()
  (let ((x  5))
    (bar)
    x))

(foo) ; => 7
