
          global    _start

; x86-64 ->
; 8 general-purpose registers extended form old x86's 8 registers
; 8 new 64-bits registers

; move dest, source

          section   .text


_start:   mov       rax, 1    ; rax -> syscall number
          mov       rdi, 1    ; rdi -> destination in stream operation
          mov       rsi, msg2 ; rsi -> source in stream operation
          mov       rsi, msg1 ; !-> msg1 is appended to msg2
          mov       rdx, 50   ; 30 -> number of bytes (more than enough, 0 are copied maybe)
                              ;
          syscall
          mov       rax, 60   ; rax -> systemd call number
                              ; 60 -> exit
          xor       rdi, rdi  ; xor -> if A and B are not equal
                              ; rdi = rdi -> always 0
                              ; program return 0
          syscall


          section   .data

msg1:  db `Hello,World!\n`
;         ^ note the grave accent ` -> enable black quote escape

; alternativly
msg2:  db "Hello,World with extra bits!", 10
;                        ^ 10 means ascii 10
