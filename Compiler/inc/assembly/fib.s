	.global main
	.text
main:
	mov $0, %rax  # prev
	mov $1, %rbx  # cur
	mov $15, %rcx # counter
fib:
	push %rax
	push %rbx
	push %rcx

	mov $format, %rdi # printf's 1st arg
	mov %rax, %rsi    # printf's 2nd arg
	mov %rbx, %rdx    # printf's 3rd arg
	mov $0, %rax      # vaargs
	
	call printf

	pop %rcx
	pop %rbx
	pop %rax
	
	mov %rax, %rdx # old prev
	mov %rbx, %rax # move cur to prev
	add %rdx, %rbx # add old prev to new cur
	
	dec %rcx
	jnz fib
	
	ret
format:
	.asciz "prev -> %-4d | cur -> %-4d\n"
	
