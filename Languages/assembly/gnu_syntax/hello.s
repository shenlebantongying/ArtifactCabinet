    .global main
    .text

main:
#gas mnemonic source,destination
    mov     $1, %rax # system call 1 -> write
    #       ^ immediate operands are preceded by $
    mov     $1, %rdi # rdi -> 1st arg -> stdout (char *buf)
    mov     $message, %rsi # 2nd arg -> size_t count
    mov     $13, %rdx
    syscall

    mov $60, %rax
    xor %rdi,%rdi # always return 0
    syscall

message:
    .ascii "Hello, World\n"
