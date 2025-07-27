#lang racket

(define (r in)
  (let ([b (read-byte)])
    (if (eq? b eof)
        (display "<end of file>")
        (begin
          (writeln (number->string b 16))
          (r in)))))

(with-input-from-file "ok.txt" (lambda () (r (current-input-port))))
