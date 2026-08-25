#lang racket
(require ffi2)

(define blas-lib (ffi2-lib "/opt/homebrew/opt/openblas/lib/libopenblas"))


(define-ffi2-type
  lapack_complex_float (struct_t
                        [real float_t]
                        [imag float_t]))


(define lapack_make_complex_float
  (ffi2-cast (ffi2-lib-ref blas-lib "lapack_make_complex_float")
             #:to (float_t float_t . -> . lapack_complex_float)))


(define scabs1
  (ffi2-cast (ffi2-lib-ref blas-lib "scabs1_")
             #:to (lapack_complex_float . -> . float_t)))


(writeln (scabs1 (lapack_make_complex_float 1.0 2.0)))
