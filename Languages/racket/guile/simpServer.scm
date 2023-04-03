;; -*- geiser-scheme-implementation: guile -*-

(use-modules (web server))

(define (hello-world-handler request request-body)
  (values '((content-type . (text/plain)))
          "Hello Worlddd!"))

(run-server hello-world-handler)
