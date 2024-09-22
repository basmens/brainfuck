.global brainfuck

.include "macro.s"

.data
src: .skip 2048, 0
out_format: .word 0
runtime_memory: .skip 30000, 0

.text


/*
	brainfuck
	Parameters:
		%rdi, The string to run as brainfuck code
	Returns: void
	Description:
		Compiles and runs the string of code
*/
brainfuck:
	PROLOGUE

	call compile
	call run
	movq $0, %rax

	EPILOGUE

/*
	compile
	Parameters:
		%rdi, The string to compile
	Returns: void
	Description:
		Compiles the code and outputs it in $src
*/
compile:
	PROLOGUE

	# Free up %r12-15
	push %r12 # -8  Becomes program string
	push %r13 # -16 Becomes loop counter
	push %r14 # -24 Becomes src counter
	push %r15 # -32

	/* bit representation in ascii of the 8 instructions
		+: 0010 1011
		-: 0010 1101
		<: 0011 1100
		>: 0011 1110
		.: 0010 1100
		,: 0010 1110
		[: 0101 1011
		]: 0101 1101

		\n: 0000 1010
		CR: 0000 1101
		NUL: 0000 0000
		Space: 0010 0000
		Multiplying by 3505308283979744561L makes the highest four bits unique, with 14 as highest index
	*/

	# Push magic number for switch statement onto stack and keep it aligned
	movq $3505308283979744561L, %rax
	pushq %rax # -40 Magic number
	subq $8, %rsp

	# Move program string into %r12, set loop counter to -1 and src counter to $src
	movq %rdi, %r12
	movq $-1, %r13
	movq $src, %r14
compile_loop:
	# Increment loop counter
	incq %r13

	# Get char
	movzb (%r12, %r13), %rax

	# Jmp into switch statement
	mulq -40(%rbp)
	shrq $60, %rax
	shlq $3, %rax # Multiply by 8
	jmp *compile_jmp_table(%rax)

compile_return:
	# Restore %r12-15
	movq -8(%rbp), %r12
	movq -16(%rbp), %r13
	movq -24(%rbp), %r14
	movq -32(%rbp), %r15
	EPILOGUE
	
compile_jmp_table:
	.quad compile_return 	# 0, zero termination
	.quad compile_loop 			# 1, space
	.quad compile_plus 	# 2, +
	.quad compile_return 	# 3
	.quad compile_if		# 4, [
	.quad compile_in		# 5, ,
	.quad compile_left 	# 6, <
	.quad compile_loop 			# 7, carriage return
	.quad compile_minus	# 8, -
	.quad compile_return	# 9
	.quad compile_for		# 10, ]
	.quad compile_out		# 11, .
	.quad compile_right	# 12, >
	.quad compile_return	# 13
	.quad compile_loop			# 14, new line


compile_plus:
	movq $1, (%r14)
	incq %r14
	jmp compile_loop

compile_minus:
	movq $2, (%r14)
	incq %r14
	jmp compile_loop

compile_left:
	movq $3, (%r14)
	incq %r14
	jmp compile_loop

compile_right:
	movq $4, (%r14)
	incq %r14
	jmp compile_loop

compile_if:
	movq $5, (%r14)
	incq %r14
	jmp compile_loop

compile_for:
	movq $6, (%r14)
	incq %r14
	jmp compile_loop

compile_out:
	movq $7, (%r14)
	incq %r14
	jmp compile_loop

compile_in:
	movq $8, (%r14)
	incq %r14
	jmp compile_loop




/*
	run
	Parameters: none
	Returns: void
	Description:
		Runs the compiles brainfuck code located in $src
*/
run:
	PROLOGUE

	# Free up %r12-15
	push %r12 # -8  Becomes src counter
	push %r13 # -16 Becomes memory pointer
	push %r14 # -24
	push %r15 # -32

	# Init src counter at -1 and memory pointer to 0
	movq $-1, %r12
	movq $0, %r13
run_loop:
	# Increment loop counter
	incq %r12

	# Jump into instruction table
	movzb src(%r12), %rax # Get instruction
	shlq $3, %rax
	jmp *run_instruction_jmp_table(%rax)

run_return:
	# Restore %r12-15
	movq -8(%rbp), %r12
	movq -16(%rbp), %r13
	movq -24(%rbp), %r14
	movq -32(%rbp), %r15
	EPILOGUE


run_instruction_jmp_table:
	.quad run_return	# 0 exit
	.quad run_instruction_plus		# 1 plus
	.quad run_instruction_minus		# 2 minus
	.quad run_instruction_left		# 3 left
	.quad run_instruction_right		# 4 right
	.quad run_instruction_if		# 5 if
	.quad run_instruction_for		# 6 for
	.quad run_instruction_in		# 7 in
	.quad run_instruction_out		# 8 out


run_instruction_plus:
	movq $0, %rax
	movb $0x2b, out_format
	movq $out_format, %rdi
	call printf
	jmp run_loop
	
run_instruction_minus:
	movq $0, %rax
	movb $0x2d, out_format
	movq $out_format, %rdi
	call printf
	jmp run_loop
	
run_instruction_left:
	movq $0, %rax
	movb $0x3c, out_format
	movq $out_format, %rdi
	call printf
	jmp run_loop
	
run_instruction_right:
	movq $0, %rax
	movb $0x3e, out_format
	movq $out_format, %rdi
	call printf
	jmp run_loop
	
run_instruction_if:
	movq $0, %rax
	movb $0x5b, out_format
	movq $out_format, %rdi
	call printf
	jmp run_loop
	
run_instruction_for:
	movq $0, %rax
	movb $0x5d, out_format
	movq $out_format, %rdi
	call printf
	jmp run_loop
	
run_instruction_in:
	movq $0, %rax
	movb $0x2e, out_format
	movq $out_format, %rdi
	call printf
	jmp run_loop

run_instruction_out:
	movq $0, %rax
	movb $0x2c, out_format
	movq $out_format, %rdi
	call printf
	jmp run_loop
