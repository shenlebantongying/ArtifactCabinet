	.global main
	.text
main:
	call printStackBasePtr
	call printStackBasePtr

	pushq $123

	call printStackBasePtr

	popq %rbx

	mov $0,%rax
	ret

printStackBasePtr:
	pushq %rbp  # rbp & rsp are callee-save
	movq %rsp, %rbp

	mov $format, %rdi
	mov %rsp, %rsi
	mov $0, %rax
	call printf

	movq %rbp, %rsp
	popq %rbp
	ret

format:
	.asciz "%p\n"
