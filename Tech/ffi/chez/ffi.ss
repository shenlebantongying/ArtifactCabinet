(library (ffi)
  (export
   load-dynamic-lib
   def-cfun)
  (import (chezscheme))

  ;; load-shared-object but with (machine-type)
  (define (load-dynamic-lib libname)
    (let* ((ext (case(machine-type)
                  [(a6le ta6le) (string-append libname ".so")]
                  [(arm64osx tarm64osx) ".dylib"]
                  [else (display-string "PUT MORE MACHINETYPE")]))
           (libname (string-append libname ext)))
      (load-shared-object libname)))

  (define-syntax def-cfun
    (syntax-rules ()
      ((_  name args -> ret)
       (define name
         (foreign-procedure (symbol->string 'name) args ret)))))
  )
