;; -*- geiser-scheme-implementation: chez -*-

;; CC => Current-Continuation

;; Apply the current-continuation to a variable
(+ 1 (call/cc
      (lambda (k)
        (+ 2 (k 3))))) ;; =>4
;; the continuation is (+1 [])
;; the cc is applied to value 3,
;; the surrounding code are irrelevant

;; from guile manual
;; Snapshot the CC, and set to knot, then reuse later.
(define kont #f)
(format #t "the return is ~a\n"
        (call/cc (lambda (k)
                   (set! kont k)
                   1)))
(kont 2)

;; from tsp4
;; once it encounter 0, will
(define product
  (lambda (ls)
    (call/cc
      (lambda (break)
        (let f ([ls ls])
          (cond
            [(null? ls) 1]
            [(= (car ls) 0) (break 3)] ;; if encounter 0, "break" with value 3 to CC
            [else (* (car ls) (f (cdr ls)))]))))))

(product '(1 2 3 4 5)) ;; 120
(product '(7 3 8 0 1 9 5)) ;; 3

;; If use CC to the toplevel, the continuation will simply be the rest of program
(call-with-current-continuation
  (lambda (exit)
    (for-each (lambda (x)
                (when (negative? x)
                    (exit x)))
              '(54 0 37 -3 245 19))
   #t))   ;; => -3
