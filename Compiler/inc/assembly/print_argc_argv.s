	.global main
	.text
main:
	# rdi -> 1st arg of main(argc....)
	mov %rdi, %rsi    # printf's 2st arg
	mov $format, %rdi # printf's 1st arg
	mov $0, %rax      # varargs
	call printf

	mov $0, %rax # set return to 0
	ret

format:
	.asciz "%d\n"
