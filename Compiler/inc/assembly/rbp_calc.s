	.global main
main:
	movq $1234, %rdi
	movq $4321, %rsi
	
	call calc

	mov %rax, %rsi
	mov $format, %rdi
	mov $0, %rax
	call printf
	
	// return val is calc's result
	ret

calc:   
	/*
	calc(int a, int b) -> int

	calling convection:

	rbp -> the frame pointer
	rsp -> pointer to the top of the frame

	-> save rbp to recover its value when returning
	-> save rsp's value immediately when entering the function
	becuase, rsp's value will change due to push & pop
	but, we want to access variables relative to a const location
	thus, rbp
	*/ 
	pushq %rbp
	movq %rsp, %rbp	
	
	movq %rdi, -8(%rbp)
	movq %rsi, -16(%rbp)

	movq -8(%rbp), %r10
	movq -16(%rbp), %r11

	addq %r10, %r11
	
	movq %r11, %rax
	
	movq %rbp, %rsp
	popq %rbp
	ret
	
format:
	.asciz "%ld\n"		
