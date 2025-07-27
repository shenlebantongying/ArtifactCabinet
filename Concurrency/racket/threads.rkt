#!racket

(define worker
  (thread (lambda ()
            (let loop ()
              (displayln "Working...")
              (sleep 0.2)
              (loop)))))

(sleep 2.5)

(kill-thread worker)
