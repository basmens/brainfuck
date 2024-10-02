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

# Comment out here to switch on/off decompiling
.macro DECOMPILER
	jmp decomile_intermediate_src
.endm

decompile_instruction_table:
	.ascii "e"	# 0 exit
	.byte 0
	.ascii "["	# 1 if
	.byte INSTRUCTION_SIZE_IF
	.ascii "]"	# 2 for
	.byte INSTRUCTION_SIZE_FOR
	.ascii ">"	# 3 right
	.byte INSTRUCTION_SIZE_RIGHT
	.ascii "e"	# 4
	.byte 0
	.ascii "e"	# 5
	.byte 0
	.ascii "e"	# 6
	.byte 0
	.ascii "e"	# 7
	.byte 0
	.ascii "e"	# 8
	.byte 0
	.ascii "e"	# 9
	.byte 0
	.ascii "e"	# 10
	.byte 0
	.ascii "e"	# 11
	.byte 0
	.ascii "e"	# 12
	.byte 0
	.ascii "e"	# 13
	.byte 0
	.ascii "e"	# 14
	.byte 0
	.ascii "e"	# 15
	.byte 0
	.ascii "e"	# 16
	.byte 0
	.ascii "e"	# 17
	.byte 0
	.ascii "e"	# 18
	.byte 0
	.ascii "e"	# 19
	.byte 0
	.ascii "e"	# 20
	.byte 0
	.ascii "e"	# 21
	.byte 0
	.ascii "e"	# 22
	.byte 0
	.ascii "e"	# 23
	.byte 0
	.ascii "e"	# 24
	.byte 0
	.ascii "e"	# 25
	.byte 0
	.ascii "i"	# 26 init mult
	.byte INSTRUCTION_SIZE_INIT_MULT
	.ascii "+"	# 27 plus
	.byte INSTRUCTION_SIZE_PLUS
	.ascii "0"	# 28 set zero
	.byte INSTRUCTION_SIZE_SET_ZERO
	.ascii "*"	# 29 mult add
	.byte INSTRUCTION_SIZE_MULT_ADD
	.ascii ","	# 30 in
	.byte INSTRUCTION_SIZE_IN
	.ascii "."	# 31 out
	.byte INSTRUCTION_SIZE_OUT

write_file_mode: .asciz "w"
write_file_name: .asciz "stats/intermediate_src.txt"
decomile_intermediate_src:
	# Align stack by popping magic number
	addq  $8, %rsp

	# Open file
	movq $write_file_name, %rdi
	movq $write_file_mode, %rsi
	call fopen
	movq %rax, %r13

	# Write file loop
	movq $0, %r12
decompile_loop:
	movzb intermediate_src(%r12), %r14
	shlq $1, %r14 # Multiply by 2
	movw decompile_instruction_table(%r14), %ax
	movzb %al, %rdi
	movb %ah, %al
	movzb %al, %rdx
	addq %rdx, %r12

	# Write to file
	movq %r13, %rsi
	call fputc

	# End condition
	cmpq $0, %r14
	jne decompile_loop

	# Close file
	movq %r13, %rdi
	call fclose

	# Epilogue of compile subroutine
	movq -8(%rbp), %r12
	movq -16(%rbp), %r13
	movq -24(%rbp), %r14
	movq -32(%rbp), %r15
	EPILOGUE

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
format_src: .asciz "%lu\n"
brainfuck:
	PROLOGUE

	// pushq %rdi
	// pushq %rdi
	// movq $0, %rax
	// movq $format_src, %rdi
	// movq $intermediate_src, %rsi
	// call printf
	// popq %rdi
	// popq %rdi

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

		The first quad contains 4 bytes for the address of the [ and 4 byte for the total mem pointer movement.
		The second quad 1 byte containing the flags for possible loop optimizations.
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
	pushq $3

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
	# Write exit instruction
	movb $OP_CODE_EXIT, intermediate_src(%r12)

	SET_INTERMEDIATE_SRC_SIZE_STAT # Comment out above
	DECOMPILER # Comment out above

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

.macro write_instruction_long name address
	movl \address, 1 + intermediate_src(%r12) # Insert address into instruction
	write_instruction \name
.endm

.macro write_instruction_byte_long name amount address
	movb \amount, 1 + intermediate_src(%r12) # Insert amount into instruction
	movl \address, 2 + intermediate_src(%r12) # Insert address into instruction
	write_instruction \name
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

.macro remove_move_instruction_via_r8 src_pointer
	# Check if there is a move instruction the if to remove
	movq intermediate_src - INSTRUCTION_SIZE_RIGHT(\src_pointer), %r8 # Get instruction before if
	cmpb $OP_CODE_RIGHT, %r8b # Check if instruction is move instruction
	jne 1f

	# Remove move instruction
	shrq $8, %r8 # Get move amount
	addl %r8d, 12(%rsp) # Increment memory pointer offset by move amount
	subl $INSTRUCTION_SIZE_RIGHT, \src_pointer # Move given src pointer back
1:
.endm

.equ LOOP_OPPTIMIZATION_SET_ZERO, 1
.equ LOOP_OPPTIMIZATION_MULT, 2
.equ LOOP_OPPTIMIZATION_SCAN_MEMORY, 4
.equ INSTRUCTION_SIZE_IF, 5
compile_if:
	compile_pop_mem_movement

	pushq %r12 # Push new empty bracket frame
	pushq $7
	write_instruction IF

	jmp compile_loop

.equ INSTRUCTION_SIZE_FOR, 5
.equ INSTRUCTION_SIZE_SET_ZERO, 5
# Multiplication first loads the amount the addition is repeated into a dedicated register, 
# then adds the mult factor * this repetition amount to the memory at the memory pointer offset.
.equ INSTRUCTION_SIZE_INIT_MULT, 6 # 1 byte for op code, 1 byte for the source add count, 4 bytes for memory pointer offset for the source
.equ INSTRUCTION_SIZE_MULT_ADD, 6 # 1 byte for op code, 1 byte for the mult factor, 4 bytes for memory pointer offset for the destination
compile_for:
	movb (%rsp), %al # Get loop optimization flags from bracket frame
	movb %al, %ah # Both al and ah contain the flags, al is used to check, ah for quick access
	andb $LOOP_OPPTIMIZATION_SET_ZERO, %al # Isolate set zero flag
	cmpb $0, %al # If set zero flag is not set, skip optimize loop
	je check_loop_optimization_mult

	# If we get here, that means that the loop contains only one plus instruction. So we skip 
	# INSTRUCTION_SIZE_IF + INSTRUCTION_SIZE_PLUS bytes back and insert a set zero instruction.
	addq $16, %rsp # Pop bracket frame
	subq $INSTRUCTION_SIZE_IF + INSTRUCTION_SIZE_PLUS, %r12 # Move intermediate src pointer back
	remove_move_instruction_via_r8 %r12d # Remove move instruction using %r12d as the src pointer

	movl 12(%rsp), %eax # Write offset from memory pointer
	write_instruction_long SET_ZERO %eax
	andb $0, (%rsp) # No loop optimizations possible
	jmp compile_loop

check_loop_optimization_mult:
	movb %ah, %al # Copy flags to al
	andb $LOOP_OPPTIMIZATION_MULT, %al # Isolate mult flag
	cmpb $0, %al # If mult flag is not set, skip optimize loop
	je check_loop_optimization_scan_memory

	# Check if the total memory movement is 0, if so, skip optimize loop.
	cmpl $0, 12(%rsp)
	jne check_loop_optimization_scan_memory

	/*
		If we got here, that means the loop contains only plus instructions and the net total memory movement is not 0.
		In other words, this loop executes a multiplication(s). First we check if we need to remove a move before the if. 
		Then we move to the start of the loop, next we check the memory pointer offset of each plus instruction. 
		If the offset is 0, we found a source add instruction and keep track of it. If the offset is not 0, 
		we found a destination and overwrite the instruction we just read with a mult instruction. %rax will contain the 
		location of the if instruction, and keep it. %rcx will contain the instruction after the if and will increment 
		to read the plus instructins. %12 will start of INSTRUCTION_SIZE_INIT_MULT away from the if statement to keep room 
		for the init instruction, and then write the mult instructions. %rdx will contain the location of the for instruction 
		to keep track of the end of the loop. Finally, %r11 will store the memory pointer offset originating form the parent 
		bracket frame and %r8 will keep track of the source add count.
	*/

	# Assignments before the move check
	movl 8(%rsp), %eax # Get address of if instruction, %rax will keep it
	movl %eax, %ecx # Copy %rax to %rcx, %rcx will increment to read the plus instructins
	addl $INSTRUCTION_SIZE_IF, %ecx # Move %rcx by INSTRUCTION_SIZE_IF
	addq $16, %rsp # Pop bracket frame

	# Reemove move instruction using %eax as the src pointer
	remove_move_instruction_via_r8 %eax

optimize_mult_loop_skip_remove_right_move:
	# Assignments after the move check
	movl %r12d, %edx # Copy %r12 to %rdx, %rdx will keep track of the end of the loop
	movl %eax, %r12d # Move intermediate src pointer to instruction after if
	addl $INSTRUCTION_SIZE_INIT_MULT, %r12d # Move %r12 by INSTRUCTION_SIZE_INIT_MULT
	movl 12(%rsp), %r11d # Write offset from memory pointer
	movl $0, %r8d # Set source add count to 0
optimize_mult_loop_loop:
	# Read plus instruction
	movq 1 + intermediate_src(%ecx), %r9 # Read plus instruction payload into %r9, %r9 will contain the amount
	movq %r9, %r10 # Copy %r9 to %r10, %r10 will contain the memory pointer offset
	shr $8, %r10 # Get memory pointer offset

	# Check if the memory pointer offset of plus instruction is 0
	cmpl $0, %r10d # If memory pointer offset is 0, it is a source add instruction
	jne optimize_mult_loop_not_source_add

	# Read the source add count
	addb %r9b, %r8b # Add source add count to source add count
	jmp optimize_mult_loop_loop_end_condition

optimize_mult_loop_not_source_add:
	# Write mult instruction
	addl %r11d, %r10d # Add memory pointer offset of bracket frame to this memory pointer offset
	write_instruction_byte_long MULT_ADD %r9b %r10d
	jmp optimize_mult_loop_loop_end_condition

optimize_mult_loop_loop_end_condition:
	# End condition
	addl $INSTRUCTION_SIZE_PLUS, %ecx # Increment %rcx by INSTRUCTION_SIZE_PLUS
	cmpl %ecx, %edx # If we haven't reached the if instruction, keep looping
	jne optimize_mult_loop_loop

	# Write init mult instruction
	movb $OP_CODE_INIT_MULT, intermediate_src(%eax) # Write op code to instruction
	movb %r8b, 1 + intermediate_src(%eax) # Write source add count to instruction
	movl %r11d, 2 + intermediate_src(%eax)

	# Write set zero instruction
	write_instruction_long SET_ZERO %r11d
	andb $0, (%rsp) # No loop optimizations possible
	jmp compile_loop


check_loop_optimization_scan_memory:
	movb %ah, %al # Copy flags to al
	andb $LOOP_OPPTIMIZATION_SCAN_MEMORY, %al # Isolate scan memory flag
	cmpb $0, %al # If scan memory flag is not set, skip optimize loop
	je compile_for_no_optimizations

compile_for_no_optimizations:
	compile_pop_mem_movement

	movl 8(%rsp), %edx # Get address of if instruction

	addq $16, %rsp # Pop bracket frame
	write_instruction FOR

	addl $INSTRUCTION_SIZE_IF, %edx # Move address to instruction after if
	movl %r12d, 1 + intermediate_src - INSTRUCTION_SIZE_IF(%edx) # Insert address after for into if instruction, 1 byte next to if instruction address
	movl %edx, 1 + intermediate_src - INSTRUCTION_SIZE_FOR(%r12) # Insert address after if instruction into current instruction

	andb $0, (%rsp) # No loop optimizations possible
	jmp compile_loop

compile_left:
	subl %r15d, 12(%rsp) # Decrement memory pointer offset in bracket frame
	andb $(~LOOP_OPPTIMIZATION_SET_ZERO), (%rsp) # Not a set zero loop
	jmp compile_loop

compile_right:
	addl %r15d, 12(%rsp) # Increment memory pointer offset in bracket frame
	andb $(~LOOP_OPPTIMIZATION_SET_ZERO), (%rsp) # Not a set zero loop
	jmp compile_loop

.equ INSTRUCTION_SIZE_PLUS, 6
/*
	Plus start with an op code, then 1 byte for the length, and then blocks of 3 representing all the plus
	instructions, each 1 byte for the amount to add, and then 2 bytes for the offset within memory.
	Minus instruction are just plus instructions, just with a negative amount to add.
*/
compile_minus:
	negb %r15b # Invert amount to subtract, now amount to add
compile_plus:
	# Init plus instruction
	movl 12(%rsp), %eax # Get offset from memory pointer
	write_instruction_byte_long PLUS %r15b %eax
	
	andb $(~LOOP_OPPTIMIZATION_SCAN_MEMORY), (%rsp) # Not a scan memory loop

	jmp compile_loop


.equ INSTRUCTION_SIZE_IN, 5
compile_in:
	movl 12(%rsp), %eax # Get offset from memory pointer
	write_instruction_long IN %eax
	andb $0, (%rsp) # No loop optimizations possible
	jmp compile_loop

.equ INSTRUCTION_SIZE_OUT, 5
compile_out:
	movl 12(%rsp), %eax # Get offset from memory pointer
	write_instruction_long OUT %eax
	andb $0, (%rsp) # No loop optimizations possible
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
	push %r15 # -32 Becomes variable register for some instructions
	# %rcx is used as instruction register

	# Init intermediate_src counter, memory pointer and output counter to 0
	movq $0, %r12
	movq $0, %r13
	movq $0, %r14
run_loop:
	INCR_EXECUTED_OPERATIONS_STAT # Comment out above

	# Jump into instruction table
	movq intermediate_src(%r12), %rcx # Get instruction
	movzb %cl, %rdx # Read op code
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
.equ OP_CODE_INIT_MULT, 26
.equ OP_CODE_PLUS, 27
.equ OP_CODE_SET_ZERO, 28
.equ OP_CODE_MULT_ADD, 29
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
	.quad run_instruction_init_mult # 26 init mult
	.quad run_instruction_plus		# 27 plus
	.quad run_instruction_set_zero	# 28 set zero
	.quad run_instruction_mult_add	# 29 mult add
	.quad run_instruction_in		# 30 in
	.quad run_instruction_out		# 31 out
	
run_instruction_if:
	addq $INSTRUCTION_SIZE_IF, %r12 # Increment intermediate src pointer regardless
	cmpb $0, runtime_memory(%r13) # Check if it needs to jump
	jne run_loop # No jump, just continue with next instruction

	shrq $8, %rcx # Get jump address
	movl %ecx, %r12d # Jump to instruction after if
	jmp run_loop
	
run_instruction_for:
	addq $INSTRUCTION_SIZE_FOR, %r12 # Increment intermediate src pointer regardless
	cmpb $0, runtime_memory(%r13) # Check if it needs to jump
	je run_loop # No jump, just continue with next instruction

	shrq $8, %rcx # Get jump address
	movl %ecx, %r12d # Jump to instruction after for
	jmp run_loop
	
run_instruction_right:
	shrq $8, %rcx # Get ammount to move
	addl %ecx, %r13d # Get ammount to move and add to memory pointer
	addl $INSTRUCTION_SIZE_RIGHT, %r12d # Increment intermediate src pointer
	jmp run_loop

run_instruction_plus:
	movb %ch, %al # Keep ammount to add
	shrq $16, %rcx # Get memory pointer offset
	addb %al, runtime_memory(%r13d, %ecx) # Add
	addq $INSTRUCTION_SIZE_PLUS, %r12 # Increment intermediate src pointer
	jmp run_loop
	
run_instruction_set_zero:
	shrq $8, %rcx # Get memory pointer offset
	movb $0, runtime_memory(%r13d, %ecx) # Set to 0
	addq $INSTRUCTION_SIZE_SET_ZERO, %r12 # Increment intermediate src pointer
	jmp run_loop

run_instruction_init_mult:
	addq $INSTRUCTION_SIZE_INIT_MULT, %r12 # Increment intermediate src pointer
	cmpb $-1, %ch # Check if add count is 1, then repetition count is x
	jne init_mult_check_one

	shrq $16, %rcx # Get memory pointer offset
	movb runtime_memory(%r13d, %ecx), %r15b # Save repetition count
	jmp run_loop

init_mult_check_one:
	cmpb $1, %ch # Check if add count is 1, then repetition count is 256 - x
	jne init_mult_non_standard_case

	shrq $16, %rcx # Get memory pointer offset
	movb runtime_memory(%r13d, %ecx), %r15b # Save repetition count
	negb %r15b # Do 256 - x
	jmp run_loop

init_mult_non_standard_case:
	movq $0, %r15 # Reset repetition count
	movq %rcx, %rax # Get memory pointer offset
	shrq $16, %rax
	movzb runtime_memory(%r13d, %eax), %rax # Get memory input

	cmpb $0, %ah # Check if add count is positive or negative
	jg init_mult_non_standard_case_positive

	# Case negative, negate add count
	negb %ch # Negate add count
	jmp init_mult_non_standard_case_loop

init_mult_non_standard_case_positive:
	# Case positive, negate input
	negb %al # Negate input

init_mult_non_standard_case_loop:
	# Divide input by add count
	movb $0, %ah # Clear ah
	divb %ch # Divide by add count
	addb %al, %r15b # Add result to repetition count

	# End condition
	cmpb $0, %ah # Check if remainder is 0
	je run_loop

	# There is a remainder, so wrap it around and divide again
	movb %ah, %al # Move remainder into al
	subb %ch, %al # Subtract add count
	incb %r15b # Add 1 to repetition count
	jmp init_mult_non_standard_case_loop

run_instruction_mult_add:
	movb %ch, %al # Move multiplier into mult register
	mulb %r15b # Multiply by repetition count
	shrq $16, %rcx # Get memory pointer offset
	addb %al, runtime_memory(%r13d, %ecx) # Add
	addq $INSTRUCTION_SIZE_MULT_ADD, %r12 # Increment intermediate src pointer
	jmp run_loop

run_instruction_in:
	movq %rcx, %r15 # Save input
	call getchar
	shrq $8, %r15 # Get memory pointer offset
	movb %al, runtime_memory(%r13d, %r15d) # Store input into memory
	addq $INSTRUCTION_SIZE_IN, %r12 # Increment intermediate src pointer
	jmp run_loop

run_instruction_out:
	shrq $8, %rcx # Get memory pointer offset
	movzb runtime_memory(%r13d, %ecx), %rdi # Ouput from memory
	addq $INSTRUCTION_SIZE_OUT, %r12 # Increment intermediate src pointer
	call putchar

	LIMIT_PRINT_COUNT

	jmp run_loop
