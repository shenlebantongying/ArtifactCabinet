;; TODO: blockquote syntax equivalent?
;;

(defmacro plus3 (a b c)
  "Say hello to the world!"
  `(+ ,a (+ ,b ,c 4)))

(defmacro headList (&optional a b &rest x)
  `'((,a ,b) ,x))

(headList x y z h i)
;; => ((X Y) (Z H I))

;; prime number, function version
(defun primep (num)
  (when (> num 1)
    (loop for fac from 2 to (isqrt num) never (zerop (mod num fac)))))

(defun next-prime (num)
  (loop for n from num when (primep n) return n))

;; without macro

(do ((p (next-prime 0) (next-prime (1+ p))))
;;      ^ init value   ^ next step
    ((> p 20)) ; termiante condition
  (format t "~d " p))

;; with macro

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

(do-primes (p 0 20) (format t "~d " p))
