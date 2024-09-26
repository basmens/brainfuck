.global brainfuck
.global intermediate_src
.global runtime_memory

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
intermediate_src: .skip 2048, 0
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
		Compiles the code and outputs it in $intermediate_src
*/
compile:
	PROLOGUE

	# Free up %r12-15
	push %r12 # -8  Becomes intermediate_src counter
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

	# Move intermediate_src counter to 0, set loop counter to -1 and program string into %r14
	movq $0, %r12
	movq $-1, %r13
	movq %rdi, %r14
compile_loop:
	# Increment loop counter
	incq %r13

	# Get char
	movzb (%r14, %r13), %rax

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
	movw $INSTRUCTION_IF, intermediate_src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_for:
	movq (%rsp), %rdx # Get if adress

	movq %r12, %rax # Insert current adress into if instruction
	shlq $OP_CODE_SIZE, %rax
	orw %ax, intermediate_src(%rdx)

	shlq $OP_CODE_SIZE, %rdx # Insert if adress into current instruction
	orq $INSTRUCTION_FOR, %rdx
	movw %dx, intermediate_src(%r12) # Store instruction

	addq $8, %rsp # Pop if off the stack

	addq $INSTRUCTION_SIZE, %r12 # Increment adress
	jmp compile_loop

compile_left:
	movw $INSTRUCTION_LEFT, intermediate_src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_right:
	movw $INSTRUCTION_RIGHT, intermediate_src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_plus:
	movw $INSTRUCTION_PLUS, intermediate_src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_minus:
	movw $INSTRUCTION_MINUS, intermediate_src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_in:
	movw $INSTRUCTION_IN, intermediate_src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop

compile_out:
	movw $INSTRUCTION_OUT, intermediate_src(%r12)
	addq $INSTRUCTION_SIZE, %r12
	jmp compile_loop




/*
	run
	Parameters: none
	Returns: void
	Description:
		Runs the compiles brainfuck code located in $intermediate_src
*/
run:
	PROLOGUE

	# Free up %r12-15
	push %r12 # -8  Becomes intermediate_src counter
	push %r13 # -16 Becomes memory pointer
	push %r14 # -24 Becomes output counter
	push %r15 # -32

	# Init intermediate_src counter at -1 and memory pointer to 0
	movq $-INSTRUCTION_SIZE, %r12
	movq $0, %r13
	movq $0, %r14
	movq $intermediate_src, %r15
run_loop:
	# Increment loop counter
	addq $INSTRUCTION_SIZE, %r12

	INCR_EXECUTED_OPERATIONS_STAT # Comment out above

	# Jump into instruction table
	movzw intermediate_src(%r12), %rax # Get instruction
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


.equ INSTRUCTION_EXIT, 0
.equ INSTRUCTION_IF, 1
.equ INSTRUCTION_FOR, 2
.equ INSTRUCTION_LEFT, 3
.equ INSTRUCTION_RIGHT, 4
.equ INSTRUCTION_PLUS, 5
.equ INSTRUCTION_MINUS, 6
.equ INSTRUCTION_IN, 7
.equ INSTRUCTION_OUT, 8
run_instruction_jmp_table:
	.quad run_return				# 0 exit
	.quad run_instruction_if		# 1 if
	.quad run_instruction_for		# 2 for
	.quad run_instruction_left		# 3 left
	.quad run_instruction_right		# 4 right
	.quad run_instruction_plus		# 5 plus
	.quad run_instruction_minus		# 6 minus
	.quad run_instruction_in		# 7 in
	.quad run_instruction_out		# 8 out
	
run_instruction_if:
	cmpb $0, runtime_memory(%r13)
	jne run_loop
	movzw intermediate_src(%r12), %rax
	shrq $OP_CODE_SIZE, %rax
	movq %rax, %r12
	jmp run_loop
	
run_instruction_for:
	cmpb $0, runtime_memory(%r13)
	je run_loop
	movzw intermediate_src(%r12), %rax
	shrq $OP_CODE_SIZE, %rax
	movq %rax, %r12
	jmp run_loop
	
run_instruction_left:
	subq $1, %r13
	jmp run_loop
	
run_instruction_right:
	addq $1, %r13
	jmp run_loop

run_instruction_plus:
	addb $1, runtime_memory(%r13)
	jmp run_loop
	
run_instruction_minus:
	subb $1, runtime_memory(%r13)
	jmp run_loop
	
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
