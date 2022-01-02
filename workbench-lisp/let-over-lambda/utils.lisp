;; onlisp chapter 4 -> str & symbols
(defun mkstr (&rest args)
  "writes args into a string and returns that string"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(mkstr pi " pieces of " '123)

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(symb 'ar "Madi" #\L #\L 0)
