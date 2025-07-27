#lang racket

;; Fixed-Point func ->
;; take a function that modify some data ->
;; if data stay the same -> return the value
;;                       -> pass the function again

;; There is no gurantee that the program will end now

; the func is a one step transformer
(define (fp data func)
  (local [(define p-data (func data))] ; p-data -> processed-data
         (if (eq? data p-data)
             p-data
             (fp p-data func))))

(fp 1 identity)

;; trim the length of lst to 5

(define (trim5 lst)
  (if (> (length lst) 5)
      (cdr lst)
      lst))

(fp (list 1 2 3 4 5 6 7 8) trim5)
