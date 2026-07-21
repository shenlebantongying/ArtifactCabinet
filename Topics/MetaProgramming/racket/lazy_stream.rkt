#lang racket

;; source https://web.mit.edu/6.001/6.037/

(define-syntax cons-stream
  (syntax-rules ()
    [(_ a b)
     (cons a
           (delay
             b))]))
(define stream-car car)
(define stream-cdr (lambda (s) (force (cdr s))))

;; operations

(define (first-n s n)
  (if (= n 0)
      '()
      (cons (stream-car s) (first-n (stream-cdr s) (- n 1)))))

(define (peek s)
  (first-n s 10))

;;
(define (map2-stream op s1 s2)
  (cons-stream (op (stream-car s1) (stream-car s2)) (map2-stream op (stream-cdr s1) (stream-cdr s2))))

(define (add-streams s1 s2)
  (map2-stream + s1 s2))

(define (add-streams* s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2)) (add-streams* (stream-cdr s1) (stream-cdr s2))))

;; simples

(define zeros (cons-stream 0 zeros))
(define ones (cons-stream 1 ones))

;; (1 2 3 4 5 .. )
(define ints (cons-stream 1 (add-streams ones ints)))

(peek ints)

;; multiply every elements of stream by a factor x
(define (scale-stream x s)
  (cons-stream (* x (stream-car s)) (scale-stream x (stream-cdr s))))

(peek (scale-stream 4 ints))
