;; -*- geiser-scheme-implementation: chez -*-

(define-record-type point (fields x y))
; define-record-type is an unhygienic macro
; -> the generated precedures depend on arguments's identifiers

(define p (make-point 1 2))
(point-x p)
(point? p)
