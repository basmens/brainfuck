.global brainfuck

# Prologue and epilogue
.macro PROLOGUE
    pushq %rbp
    movq %rsp, %rbp
.endm

.macro EPILOGUE
    movq %rbp, %rsp
    popq %rbp
    ret
.endm

# Parameters
.macro PARAM1 p1
    movq \p1, %rdi
.endm

.macro PARAM2 p1, p2
    movq \p1, %rdi
    movq \p2, %rsi
.endm

.macro PARAM3 p1, p2, p3
    movq \p1, %rdi
    movq \p2, %rsi
    movq \p3, %rdx
.endm

.macro PARAM4 p1, p2, p3, p4
    movq \p1, %rdi
    movq \p2, %rsi
    movq \p3, %rdx
    movq \p4, %rcx
.endm


# Comment out here to switch off statistics
.macro SET_INTERMEDIATE_SRC_SIZE_STAT
	movq %r12, %rax
	movq $0, %rdx
	movq $INSTRUCTION_SIZE, %rdi
	divq %rdi
	movq %rax, intermediate_src_size
.endm

.macro INCR_EXECUTED_OPERATIONS_STAT
	incq executed_operations
.endm


.equ INSTRUCTION_SIZE, 2 # In bytes
.equ OP_CODE_SIZE, 5 # In bits
.equ OP_CODE_BIT_MASK, 0x1f



.data
src: .skip 2048, 0
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
	push %r12 # -8  Becomes src counter
	push %r13 # -16 Becomes loop counter
	push %r14 # -24 Becomes program string
	push %r15 # -32 Becomes instruction repetition counter

	/* bit representation in ascii of the 8 instructions
		+: 0010 1011
		-: 0010 1101
		<: 0011 1100
		>: 0011 1110
		[: 0101 1011
		]: 0101 1101
		.: 0010 1100
		,: 0010 1110

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

	# Move src counter to 0, set loop counter to -1 and program string into %r14
	movq $0, %r12
	movq $-1, %r13
	movq %rdi, %r14
compile_loop:
	# Increment loop counter
	incq %r13

	# Get char
	movzb (%r14, %r13), %rax
	
	// TMP CODE
	cmpb $0x5b, %al
	je skip_char_counter
	cmpb $0x5d, %al
	je skip_char_counter
	cmpb $0x2c, %al
	je skip_char_counter
	cmpb $0x2e, %al
	je skip_char_counter

	movq $0, %r15
	decq %r13
char_counter_loop:
	incq %r13
	incq %r15
	cmpb 1(%r14, %r13), %al
	je char_counter_loop
skip_char_counter:
	//

	# Jmp into switch statement
	mulq -40(%rbp)
	shrq $60, %rax
	shlq $3, %rax # Multiply by 8
	jmp *compile_jmp_table(%rax)

compile_return:
	SET_INTERMEDIATE_SRC_SIZE_STAT # Comment out above

	# Restore %r12-15
	movq -8(%rbp), %r12
	movq -16(%rbp), %r13
	movq -24(%rbp), %r14
	movq -32(%rbp), %r15
	EPILOGUE
	
compile_jmp_table:
	.quad compile_return 	# 0, zero termination
	.quad compile_loop 		# 1, space
	.quad compile_plus		# 2, +
	.quad compile_return 	# 3
	.quad compile_if		# 4, [
	.quad compile_in		# 5, ,
	.quad compile_left		# 6, <
	.quad compile_loop		# 7, carriage return
	.quad compile_minus		# 8, -
	.quad compile_return	# 9
	.quad compile_for		# 10, ]
	.quad compile_out		# 11, .
	.quad compile_right		# 12, >
	.quad compile_return	# 13
	.quad compile_loop		# 14, new line

/*
	Compiles brainfuck program into 2 byte instructions. Each instruction contains a the instruction itself
	in the 5 least significant bits. Then 11 bits are reserved for parameters.
*/

compile_if:
	pushq %r12
	movw $INSTRUCTION_IF, src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_for:
	movq (%rsp), %rdx # Get if adress

	movq %r12, %rax # Insert current adress into if instruction
	shlq $3, %rax
	orw %ax, src(%rdx)

	shlq $3, %rdx # Insert if adress into current instruction
	orq $INSTRUCTION_FOR, %rdx
	movw %dx, src(%r12) # Store instruction

	addq $8, %rsp # Pop if off the stack

	addq $INSTRUCTION_SIZE, %r12 # Increment adress
	jmp compile_loop

compile_left:
	movw $INSTRUCTION_LEFT, src(%r12)
	shlq $OP_CODE_SIZE, %r15
	orw %r15w, src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_right:
	movw $INSTRUCTION_RIGHT, src(%r12)
	shlq $OP_CODE_SIZE, %r15
	orw %r15w, src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_plus:
	movw $INSTRUCTION_PLUS, src(%r12)
	shlq $OP_CODE_SIZE, %r15
	orw %r15w, src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_minus:
	movw $INSTRUCTION_MINUS, src(%r12)
	shlq $OP_CODE_SIZE, %r15
	orw %r15w, src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_in:
	movw $INSTRUCTION_IN, src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_out:
	movw $INSTRUCTION_OUT, src(%r12)
	addq $INSTRUCTION_SIZE, %r12
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
	push %r14 # -24 Becomes output counter
	push %r15 # -32

	# Init src counter at -1 and memory pointer to 0
	movq $-INSTRUCTION_SIZE, %r12
	movq $0, %r13
	movq $0, %r14
	movq $src, %r15
run_loop:
	# Increment loop counter
	addq $INSTRUCTION_SIZE, %r12

	INCR_EXECUTED_OPERATIONS_STAT # Comment out above

	# Jump into instruction table
	movzw src(%r12), %rax # Get instruction
	andq $OP_CODE_BIT_MASK, %rax
	shlq $3, %rax
	jmp *run_instruction_jmp_table(%rax)

run_return:
	# Restore %r12-15
	movq -8(%rbp), %r12
	movq -16(%rbp), %r13
	movq -24(%rbp), %r14
	movq -32(%rbp), %r15
	EPILOGUE


.equ INSTRUCTION_EXIT, 2
.equ INSTRUCTION_IF, 1
.equ INSTRUCTION_FOR, 2
.equ INSTRUCTION_LEFT, 3
.equ INSTRUCTION_RIGHT, 4
.equ INSTRUCTION_PLUS, 27
.equ INSTRUCTION_MINUS, 24
.equ INSTRUCTION_SET, 28
.equ INSTRUCTION_MULt, 29
.equ INSTRUCTION_IN, 30
.equ INSTRUCTION_OUT, 31
run_instruction_jmp_table:
	.quad run_return				# 0 exit
	.quad run_instruction_if		# 1 if xx001 is all if
	.quad run_instruction_for		# 2 if xx010 is all for
	.quad run_instruction_left		# 3 left
	.quad run_instruction_right		# 4 right
	.quad run_return				# 5
	.quad run_return				# 6
	.quad run_return				# 7
	.quad run_return				# 8
	.quad run_instruction_if		# 9  if xx001 is all if
	.quad run_instruction_for		# 10 if xx010 is all for
	.quad run_return				# 11
	.quad run_return				# 12
	.quad run_return				# 13
	.quad run_return				# 14
	.quad run_return				# 15
	.quad run_return				# 16
	.quad run_instruction_if		# 17 if xx001 is all if
	.quad run_instruction_for		# 18 if xx010 is all for
	.quad run_return				# 19
	.quad run_return				# 20
	.quad run_return				# 21
	.quad run_return				# 22
	.quad run_return				# 23
	.quad run_instruction_minus		# 24 minus
	.quad run_instruction_if		# 25 if xx000 is all if
	.quad run_instruction_for		# 26 if xx001 is all for
	.quad run_instruction_plus		# 27 plus
	.quad run_instruction_set		# 28 set
	.quad run_instruction_mult		# 29 mult
	.quad run_instruction_in		# 30 in
	.quad run_instruction_out		# 31 out
	
run_instruction_if:
	cmpb $0, runtime_memory(%r13)
	jne run_loop
	movzw src(%r12), %rax
	shrq $3, %rax
	movq %rax, %r12
	jmp run_loop
	
run_instruction_for:
	cmpb $0, runtime_memory(%r13)
	je run_loop
	movzw src(%r12), %rax
	shrq $3, %rax
	movq %rax, %r12
	jmp run_loop
	
run_instruction_left:
	movzw src(%r12), %rax
	shrq $OP_CODE_SIZE, %rax
	subq %rax, %r13
	jmp run_loop
	
run_instruction_right:
	movzw src(%r12), %rax
	shrq $OP_CODE_SIZE, %rax
	addq %rax, %r13
	jmp run_loop

run_instruction_plus:
	movzw src(%r12), %rax
	shrq $OP_CODE_SIZE, %rax
	addb %al, runtime_memory(%r13)
	jmp run_loop
	
run_instruction_minus:
	movzw src(%r12), %rax
	shrq $OP_CODE_SIZE, %rax
	subb %al, runtime_memory(%r13)
	jmp run_loop
	
run_instruction_set:
run_instruction_mult:
run_instruction_in:
	call getchar
	movb %al, runtime_memory(%r13)
	jmp run_loop

run_instruction_out:
	movq $0, %rax
	movb runtime_memory(%r13), %al
	movzb %al, %rdi
	call putchar

	// incq %r14
	// cmpq $1560, %r14
	// je run_return

	jmp run_loop
