.globl loop
loop:
	pushq  %rbp
	movq   %rsp, %rbp
	pushq  %rbp
	pushq  %rbx
	pushq  %r12
	pushq  %r13
	pushq  %r14
	pushq  %r15
	movq   %rcx, %rbp
	movq   %rdx, %rbx
	movq   %rsi, %rdx
	movq   %rdi, %rax
	lea    return(%rip), %rcx
	subq   %rax, %rcx
	pushq  %r9 # argv
	pushq  %r8 # argc
	pushq  %rcx # dummy rL
	movzwq (%rax, %rbp), %rsi
	jmp    *(%rdx, %rsi, 8)
return:
	popq   %rax
	popq   %r15
	popq   %r14
	popq   %r13
	popq   %r12
	popq   %rbx
	popq   %rbp
	leave
	ret
