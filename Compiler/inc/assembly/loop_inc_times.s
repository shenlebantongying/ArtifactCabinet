	.global main
	.text

main:
	mov $10, %rax   # counter
	mov $100, %rbx  # current num
print:
	push %rax
	push %rbx # save the values for later retrival

	mov $format, %rdi # printf's 1st arg
	mov %rbx, %rsi    # printf's 2st arg
	mov $0, %rax      # varargs
	call printf

	pop %rbx
	pop %rax

	inc %rbx

	dec %rax  # affecting ZF for jnz
	jnz print # jump if not equal to zero

	ret
format:
	.asciz "%d\n"
