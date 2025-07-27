#lang slideshow

(define (square n)
  (filled-rectangle n n))

(define (four p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(let* ([rs (colorize (square 10) "red")]
       [bs (colorize (square 10) "black")]
       [c4 (four rs bs)]
       [c16 (four c4 c4)])
  (four c16 c16))

;; note the apply can "feed" the function with a list
(define (series o n)
  (apply ht-append 4 (build-list n (lambda (x) (o (* x 10))))))

(series circle 10)
