#lang racket
(require racket)

; print every line with str lang ""
(define in-name "slb.rkt")

(for-each
  (lambda (s)
    (if (string-contains? s "lang") 
      (display (string-append s "\n")) 
      null))
  (filter non-empty-string? (file->lines in-name)) 
)



