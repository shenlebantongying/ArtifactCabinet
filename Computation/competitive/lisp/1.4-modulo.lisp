;; actually in lisp alreay -> remove-duplicates
(defun rm-duplicates (lst)
  (cond ((null lst) lst)
        ((member (car lst) (cdr lst)) (rm-duplicates (cdr lst)))
        (t (cons (car lst) (rm-duplicates (cdr lst))))))

(let ((lst '()))
  (loop for a = (read *standard-input* nil) until (null a)
        do (push (mod a 42) lst)
        finally (format t "~d ~%" (length (rm-duplicates lst)))))
