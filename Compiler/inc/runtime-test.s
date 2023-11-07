	.global main
main:
	movq $123456, %rdi
	callq print_int
	ret
