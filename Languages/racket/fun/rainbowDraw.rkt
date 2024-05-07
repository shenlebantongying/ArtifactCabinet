#lang racket
(require racket/draw)

(define o (make-object bitmap% 200 200))
(define c (new bitmap-dc% [bitmap o]))

(for* ([x (in-range 200)]
       [y (in-range 200)])
  (send c set-pixel x y
        (make-object color%
          (random 256)
          (random 256)
          (random 256))))

(send o save-file "rainbow" 'png)
