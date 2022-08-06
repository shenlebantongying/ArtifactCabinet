#lang racket

;; CC -> describe the rest of the computation
(+ (call/cc
    (lambda (k^) ;; k is an escape precedure
      (/ (k^ 5) 4))) 8)

;; k^ = (lambda^ (v) (+ v 8))

;; cc as first-class objects


(define +8^ null)

(+ (call/cc
    (lambda (k^)
      (begin
        (set! +8^ k^)
        (display "inside ->")
        5)))
 8)

(+ 1 (/ (+8^ 10) 0)) ;; => always 18, note that the outer calculations doesn't matter

;; Call/cc for early return
;; http://bewatermyfriend.org/p/2019/002/

(call/cc (lambda (return) ;; the call/cc is called with an arg
            (foldr (lambda (x acc)
                    (if (zero? x)
                      (return 0)
                      (* x acc)))
                  (*)
                  '(1 2 3 4 0 6 7 8 9 10)))) ;; this program exist early with the 5th zero

