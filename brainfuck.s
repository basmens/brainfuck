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
	movq %r12, intermediate_src_size
.endm

.macro INCR_EXECUTED_OPERATIONS_STAT
	incq executed_operations
.endm

.macro GET_TIME
	movq $228, %rax # clock_gettime
	movq $0, %rdi
	movq $compile_time_out, %rsi
	syscall
.endm


# Limit print count, use 1000 for stats
.macro LIMIT_PRINT_COUNT
	// incq %r14
	// cmpq $1000, %r14
	// je run_return
.endm



.data
intermediate_src: .skip 65546, 0
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
	GET_TIME
	call run

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

	/*
		The compiler will have a two quads pushed onto the stack for every bracket frame. So, when entering a [, a two quads will
		be pushed, and when ], will be exited, a two quads will be popped.

		// The first quad contains 4 bytes for the address of the [ and 4 byte for the total mem pointer movement.
		// The second quad 1 byte for the last operation, then 4 byte for the location of that operation.
	*/

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
	pushq $0 # Push first stack frame
	pushq $0

	# Move intermediate_src counter to 0, set loop counter to -1 and program string into %r14
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


.macro write_instruction name
	movb $OP_CODE_\name, intermediate_src(%r12) # Write op code
	addq $INSTRUCTION_SIZE_\name, %r12 # Increment intermediate src pointer by length
.endm


.equ INSTRUCTION_SIZE_RIGHT, 5
.macro compile_pop_mem_movement
	movl 12(%rsp), %eax # Get memory pointer offset from bracket frame
	cmpl $0, %eax # If memory pointer offset is 0, skip
	je 1f

	movl %eax, 1 + intermediate_src(%r12) # Insert move ammount into instruction
	movl $0, 12(%rsp) # Make memory pointer offset 0
	write_instruction RIGHT
1:
.endm



.equ INSTRUCTION_SIZE_IF, 5
compile_if:
	compile_pop_mem_movement

	pushq %r12 # Push new empty bracket frame
	pushq $0
	write_instruction IF

	jmp compile_loop

.equ INSTRUCTION_SIZE_FOR, 5
compile_for:
	compile_pop_mem_movement

	movl 8(%rsp), %edx # Get address of if instruction

	addq $16, %rsp # Pop bracket frame
	write_instruction FOR

	addl $INSTRUCTION_SIZE_IF, %edx # Move address to instruction after if
	movl %r12d, 1 + intermediate_src - INSTRUCTION_SIZE_IF(%edx) # Insert address after for into if instruction, 1 byte next to if instruction address
	movl %edx, 1 + intermediate_src - INSTRUCTION_SIZE_FOR(%r12) # Insert address after if instruction into current instruction

	jmp compile_loop

compile_left:
	subl %r15d, 12(%rsp) # Decrement memory pointer offset in bracket frame
	jmp compile_loop

compile_right:
	addl %r15d, 12(%rsp) # Increment memory pointer offset in bracket frame
	jmp compile_loop

.equ INSTRUCTION_SIZE_PLUS, 6
/*
	Plus start with an op code, then 1 byte for the length, and then blocks of 3 representing all the plus
	instructions, each 1 byte for the ammount to add, and then 2 bytes for the offset within memory.
	Minus instruction are just plus instructions, just with a negative ammount to add.
*/
compile_minus:
	negb %r15b # Invert ammount to subtract, now ammount to add
compile_plus:
	# Init plus instruction
	movb %r15b, 1 + intermediate_src(%r12) # Write ammount to add
	movl 12(%rsp), %eax # Write offset from memory pointer
	movl %eax, 2 + intermediate_src(%r12)
	write_instruction PLUS

	jmp compile_loop


.equ INSTRUCTION_SIZE_IN, 5
compile_in:
	movl 12(%rsp), %eax # Write offset from memory pointer
	movl %eax, 1 + intermediate_src(%r12)
	write_instruction IN
	jmp compile_loop

.equ INSTRUCTION_SIZE_OUT, 5
compile_out:
	movl 12(%rsp), %eax # Write offset from memory pointer
	movl %eax, 1 + intermediate_src(%r12)
	write_instruction OUT
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
	# %rax is used as instruction register

	# Init intermediate_src counter, memory pointer and output counter to 0
	movq $0, %r12
	movq $0, %r13
	movq $0, %r14
run_loop:
	INCR_EXECUTED_OPERATIONS_STAT # Comment out above

	# Jump into instruction table
	movq intermediate_src(%r12), %rax # Get instruction
	movzb %al, %rdx # Read op code
	shlq $3, %rdx # Multiply by 8
	jmp *run_instruction_jmp_table(%rdx) # Index into the table

run_return:
	# Restore %r12-15
	movq -8(%rbp), %r12
	movq -16(%rbp), %r13
	movq -24(%rbp), %r14
	movq -32(%rbp), %r15
	EPILOGUE


.equ OP_CODE_EXIT, 0
.equ OP_CODE_IF, 1
.equ OP_CODE_FOR, 2
.equ OP_CODE_RIGHT, 3
.equ OP_CODE_PLUS, 27
.equ OP_CODE_SET, 28
.equ OP_CODE_MULt, 29
.equ OP_CODE_IN, 30
.equ OP_CODE_OUT, 31
run_instruction_jmp_table:
	.quad run_return				# 0 exit
	.quad run_instruction_if		# 1 if
	.quad run_instruction_for		# 2 for
	.quad run_instruction_right		# 3 right
	.quad run_return				# 4
	.quad run_return				# 5
	.quad run_return				# 6
	.quad run_return				# 7
	.quad run_return				# 8
	.quad run_return				# 9
	.quad run_return				# 10
	.quad run_return				# 11
	.quad run_return				# 12
	.quad run_return				# 13
	.quad run_return				# 14
	.quad run_return				# 15
	.quad run_return				# 16
	.quad run_return				# 17
	.quad run_return				# 18
	.quad run_return				# 19
	.quad run_return				# 20
	.quad run_return				# 21
	.quad run_return				# 22
	.quad run_return				# 23
	.quad run_return				# 24
	.quad run_return				# 25
	.quad run_return				# 26
	.quad run_instruction_plus		# 27 plus
	.quad run_instruction_set		# 28 set
	.quad run_instruction_mult		# 29 mult
	.quad run_instruction_in		# 30 in
	.quad run_instruction_out		# 31 out
	
run_instruction_if:
	addq $INSTRUCTION_SIZE_IF, %r12 # Increment intermediate src pointer regardless
	cmpb $0, runtime_memory(%r13) # Check if it needs to jump
	jne run_loop # No jump, just continue with next instruction

	shrq $8, %rax # Get jump address
	movl %eax, %r12d # Jump to instruction after if
	jmp run_loop
	
run_instruction_for:
	addq $INSTRUCTION_SIZE_FOR, %r12 # Increment intermediate src pointer regardless
	cmpb $0, runtime_memory(%r13) # Check if it needs to jump
	je run_loop # No jump, just continue with next instruction

	shrq $8, %rax # Get jump address
	movl %eax, %r12d # Jump to instruction after for
	jmp run_loop
	
run_instruction_right:
	shrq $8, %rax # Get ammount to move
	addl %eax, %r13d # Get ammount to move and add to memory pointer
	addl $INSTRUCTION_SIZE_RIGHT, %r12d # Increment intermediate src pointer
	jmp run_loop

run_instruction_plus:
	movb %ah, %dl # Keep ammount to add
	shrq $16, %rax # Get memory pointer offset
	addb %dl, runtime_memory(%r13d, %eax) # Add
	addq $INSTRUCTION_SIZE_PLUS, %r12 # Increment intermediate src pointer
	jmp run_loop
	
run_instruction_set:
run_instruction_mult:
run_instruction_in:
	call getchar
	movl 1 + intermediate_src(%r12), %edx # Get memory pointer offset
	movb %al, runtime_memory(%r13d, %edx) # Store input into memory
	addq $INSTRUCTION_SIZE_IN, %r12 # Increment intermediate src pointer
	jmp run_loop

run_instruction_out:
	shrq $8, %rax # Get memory pointer offset
	movzb runtime_memory(%r13d, %eax), %rdi # Ouput from memory
	addq $INSTRUCTION_SIZE_OUT, %r12 # Increment intermediate src pointer
	call putchar

	LIMIT_PRINT_COUNT

	jmp run_loop
