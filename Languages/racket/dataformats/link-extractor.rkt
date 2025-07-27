#lang racket
(require json)

(for-each (λ (hq) (displayln (hash-ref hq 'link)))
          (read-json (open-input-file "webcollections.json")))
