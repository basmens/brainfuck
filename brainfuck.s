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
	incq %r14
	cmpq $1000, %r14
	je run_return
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
		The compiler will have a quad pushed onto the stack for every bracket frame. So, when entering a [, a quad will
		be pushed, and when ], will be exited, a quad will be popped.

		Each quad contains 2 bytes for the address of the [, 2 byte for the total mem pointer movement in excess notation, 1 byte for
		the last operation, then 2 byte for the location of the length of that operation.
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
	movq $0x80000000, %rax # Push first stack frame
	pushq %rax

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

/*
	Compiles brainfuck program into 2 byte instructions. Each instruction contains a the instruction itself
	in the 5 least significant bits. Then 11 bits are reserved for parameters.
*/


.macro write_instruction name
	movb $OP_CODE_\name, intermediate_src(%r12) # Write op code
	addq $INSTRUCTION_SIZE_\name, %r12 # Increment intermediate src pointer by length
	movb $OP_CODE_\name, 4(%rsp) # Write last operation to bracket frame
.endm


.equ INSTRUCTION_SIZE_RIGHT, 3
.macro compile_pop_move
	movw 2(%rsp), %ax # Get memory pointer offset from bracket frame
	cmpw $0x8000, %ax # If memory pointer offset is 0, skip
	je 1f

	movw %ax, 1 + intermediate_src(%r12) # Insert move ammount into instruction
	movw $0x8000, 2(%rsp) # Make memory pointer offset 0
	write_instruction RIGHT
1:
.endm



.equ INSTRUCTION_SIZE_IF, 3
compile_if:
	compile_pop_move

	pushq %r12 # Push new empty bracket frame
	movw $0x8000, 2(%rsp) # Make memory pointer offset 0, in excess notations
	write_instruction IF

	jmp compile_loop

.equ INSTRUCTION_SIZE_FOR, 3
compile_for:
	compile_pop_move

	movzw (%rsp), %rdx # Get address of if instruction

	addq $8, %rsp # Pop bracket frame
	write_instruction FOR

	addq $INSTRUCTION_SIZE_IF, %rdx # Increment address to after if instruction
	movw %r12w, 1 + intermediate_src - INSTRUCTION_SIZE_IF(%rdx) # Insert address after for into if instruction, 1 byte next to if instruction address
	movw %dx, 1 + intermediate_src - INSTRUCTION_SIZE_FOR(%r12) # Insert address after if instruction into current instruction

	jmp compile_loop

compile_left:
	subw %r15w, 2(%rsp) # Decrement memory pointer offset in bracket frame
	jmp compile_loop

compile_right:
	addw %r15w, 2(%rsp) # Increment memory pointer offset in bracket frame
	jmp compile_loop

.equ INSTRUCTION_SIZE_PLUS, 2
.equ INSTRUCTION_SIZE_PLUS_BLOCK, 3
/*
	Plus start with an op code, then 1 byte for the length, and then blocks of 3 representing all the plus
	instructions, each 1 byte for the ammount to add, and then 2 bytes for the offset within memory.
	Minus instruction are just plus instructions, just with a negative ammount to add.
*/
compile_plus:
	# Check if plus instruction has already been initialized
	cmpb $OP_CODE_PLUS, 4(%rsp) # Check if last operation is plus
	je compile_plus_skip_init # If so, skip init

	# Init plus instruction
	incq %r12 # Increment intermediate src pointer by 1
	movb $0, intermediate_src(%r12) # Write length of 0
	movw %r12w, 5(%rsp) # Write address of add block count
	decq %r12 # Decrement intermediate src pointer by 1
	write_instruction PLUS

compile_plus_skip_init:
	# Add add block
	movzw 5(%rsp), %rax # Get address of add block count
	incb intermediate_src(%rax) # Increment add block count
	movb %r15b, intermediate_src(%r12) # Write ammount to add
	movw 2(%rsp), %ax # Write offset from memory pointer
	movw %ax, 1 + intermediate_src(%r12)

	addq $INSTRUCTION_SIZE_PLUS_BLOCK, %r12 # Increment intermediate src pointer by add block size
	jmp compile_loop

compile_minus:
	# Check if plus instruction has already been initialized
	cmpb $OP_CODE_PLUS, 4(%rsp) # Check if last operation is plus
	je compile_minus_skip_init # If so, skip init

	# Init plus instruction
	incq %r12 # Increment intermediate src pointer by 1
	movb $0, intermediate_src(%r12) # Write length of 0
	movw %r12w, 5(%rsp) # Write address of add block count
	decq %r12 # Decrement intermediate src pointer by 1
	write_instruction PLUS

compile_minus_skip_init:
	movzw 5(%rsp), %rax # Get address of add block count
	incb intermediate_src(%rax) # Increment add block count
	negb %r15b # Invert ammount to subtract, now ammount to add
	movb %r15b, intermediate_src(%r12) # Write ammount to add
	movw 2(%rsp), %ax # Write offset from memory pointer
	movw %ax, 1 + intermediate_src(%r12)

	addq $INSTRUCTION_SIZE_PLUS_BLOCK, %r12 # Increment intermediate src pointer by add block size
	jmp compile_loop


.equ INSTRUCTION_SIZE_IN, 3
compile_in:
	movw 2(%rsp), %ax # Write offset from memory pointer
	movw %ax, 1 + intermediate_src(%r12)
	write_instruction IN
	jmp compile_loop

.equ INSTRUCTION_SIZE_OUT, 3
compile_out:
	movw 2(%rsp), %ax # Write offset from memory pointer
	movw %ax, 1 + intermediate_src(%r12)
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

	# Init intermediate_src counter, memory pointer and output counter to 0
	movq $0, %r12
	movq $0, %r13
	movq $0, %r14
	movq $intermediate_src, %r15
run_loop:
	INCR_EXECUTED_OPERATIONS_STAT # Comment out above

	# Jump into instruction table
	movzb intermediate_src(%r12), %rax # Get instruction
	shlq $3, %rax # Multiply by 8
	jmp *run_instruction_jmp_table(%rax) # Index into the table

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

	movw intermediate_src - INSTRUCTION_SIZE_IF + 1(%r12), %r12w # Jump to instruction after if
	jmp run_loop
	
run_instruction_for:
	addq $INSTRUCTION_SIZE_FOR, %r12 # Increment intermediate src pointer regardless
	cmpb $0, runtime_memory(%r13) # Check if it needs to jump
	je run_loop # No jump, just continue with next instruction

	movw intermediate_src - INSTRUCTION_SIZE_FOR + 1(%r12), %r12w # Jump to instruction after for
	jmp run_loop
	
run_instruction_right:
	movzw 1 + intermediate_src(%r12), %rax # Get ammount to move, in excess notation
	subq $0x8000, %rax # Subtract the excess, now two's compliment in a quad
	addq %rax, %r13 # Add to the memory pointer
	addq $INSTRUCTION_SIZE_RIGHT, %r12 # Increment intermediate src pointer
	jmp run_loop

run_instruction_plus:
	movzb 1 + intermediate_src(%r12), %rcx # Get ammount of add blocks
	addq $INSTRUCTION_SIZE_PLUS, %r12 # Move to first add block
run_instruction_plus_loop:
	movb intermediate_src(%r12), %al # Get ammount to add
	movzw 1 + intermediate_src(%r12), %rdx # Get offset from memory pointer
	subq $0x8000, %rdx # Remove excess from memory pointer, now two's compliment quad
	addb %al, runtime_memory(%r13, %rdx) # Add
	addq $INSTRUCTION_SIZE_PLUS_BLOCK, %r12 # Increment by one add block
	loop run_instruction_plus_loop # Loop to next add block

	jmp run_loop
	
run_instruction_set:
run_instruction_mult:
run_instruction_in:
	call getchar
	movzw 1 + intermediate_src(%r12), %rdx # Get offset from memory pointer
	subq $0x8000, %rdx # Remove excess from memory pointer, now two's compliment quad
	movb %al, runtime_memory(%r13, %rdx) # Store input into memory
	addq $INSTRUCTION_SIZE_IN, %r12 # Increment intermediate src pointer
	jmp run_loop

run_instruction_out:
	movq $0, %rax
	movzw 1 + intermediate_src(%r12), %rdx # Get offset from memory pointer
	subq $0x8000, %rdx # Remove excess from memory pointer, now two's compliment quad
	movzb runtime_memory(%r13, %rdx), %rdi # Ouput from memory
	addq $INSTRUCTION_SIZE_OUT, %r12 # Increment intermediate src pointer
	call putchar

	LIMIT_PRINT_COUNT

	jmp run_loop
