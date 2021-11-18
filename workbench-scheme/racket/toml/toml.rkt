#lang racket


(define (read-toml [i (current-input-port)])
  (begin (displayln (read-char i))
         (displayln (read-char i))
         (displayln (read-char i))))

(read-toml (open-input-file "sample.toml"))