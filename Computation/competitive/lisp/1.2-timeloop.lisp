;; https://open.kattis.com/problems/timeloop

(let ((times (read *standard-input* nil)))
    (loop for n from 1 below (1+ times) by 1
        do (format t "~a Abracadabra~%" n)))
