.global brainfuck

format_str: .asciz "We should be executing the following code:\n%s"

# Your brainfuck subroutine will receive one argument:
# a zero termianted string containing the code to execute.
brainfuck:
	pushq %rbp
	movq %rsp, %rbp

	movq %rdi, %rsi
	movq $format_str, %rdi
	call printf
	movq $0, %rax

	movq %rbp, %rsp
	popq %rbp
	ret
