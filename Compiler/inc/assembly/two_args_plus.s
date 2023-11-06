	# print 2 args
	.global main
	.text
main:
	# main (argc: rdi, argv: rsi)
	
	cmp $3, %rdi
	jne err # exit if don't have 2 args

	mov %rsi, %r12	# keep a copy of argv
	
	mov $twoargs, %rdi
	mov 8(%r12), %rsi
	mov 16(%r12), %rdx
	mov $0, %rax
	call printf

	# convert argv[1] then put it in stack
	mov 8(%r12), %rdi
	call atoi
	
	push %rax

	# for argv[2]
	mov 16(%r12), %rdi
	call atoi

	pop %rsi
	mov %rax, %rdx

	mov %rsi, %rcx
	add %rdx, %rcx
	
	mov $dfmt, %rdi

	mov $0, %rax
	call printf
	
	mov $0, %rax
	ret
err:
	mov $1, %rax
	ret
twoargs:
	.asciz "arg1 -> %s\narg2 -> %s\n"
dfmt:
	.asciz "\n%d + %d = %d\n"
