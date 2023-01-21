#lang racket

(define (fcons x y)
  (lambda (m) (m x y)))

(define (fcar z)
  (z (lambda (p q) p)))

(define (fcdr z)
  (z (lambda (p q) q)))

(fcdr (fcons 2 (fcons 1 2)))
