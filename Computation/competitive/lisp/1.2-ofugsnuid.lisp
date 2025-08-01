;; https://open.kattis.com/problems/ofugsnuid

(read *standard-input* nil)

(let ((lst '()))
(loop for a = (read *standard-input* nil) until (null a)
    do (push a lst) ;; note that push -> a to lst's head
    finally (loop for a in lst
                do (format t "~a ~%" a))))
