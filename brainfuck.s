.global brainfuck
.global runtime_memory
.global getchar # Tell the linker to link this symbol so that the executable memory can use it
.text

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


# Comment out here to switch off statistics
.macro SET_INTERMEDIATE_SRC_SIZE_STAT
	// subq -48(%rbp), %r13
	// movq %r13, intermediate_src_size
	// addq -48(%rbp), %r13
.endm

.macro INCR_EXECUTED_OPERATIONS_STAT
	// incq executed_operations
.endm

.macro GET_TIME
	// movq $228, %rax # clock_gettime
	// movq $0, %rdi
	// movq $compile_time_out, %rsi
	// syscall
.endm

# Comment out here to switch on/off decompiling
.macro DECOMPILER
	// jmp decomile_intermediate_src
.endm

# RIP decompiler since executable memory optimization
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

	# Close file
	movq %r13, %rdi
	call fclose

	# Epilogue of compile subroutine
	movq -8(%rbp), %r12
	movq -16(%rbp), %r13
	movq -24(%rbp), %r14
	movq -32(%rbp), %r15
	movq -40(%rbp), %rbx
	EPILOGUE



.data
.align 8
intermediate_src: .skip 35536, 0 # Can use the output buffer too since it is independent from the first pass of the compilation and does not require initialization
output_buffer: .skip 30000, 0
runtime_memory: .skip 30000, 0

.text



##############################################################################################################################################
# Read and first pass compilation
##############################################################################################################################################
.macro compile_char_simple char compiler_label initial_repetition_count
compile_char_\char:
	movq %r15, %rax # Copy last read instruction
	movq %rbx, %rdx # Copy instruction repetition counter into %rdx for the intructions to use
	movq $\compiler_label, %r15 # Set last read instruction
	movq $\initial_repetition_count, %rbx # Set instruction repetition counter to initial value
	cmpq $0, %rdx # If instruction repetition counter is 0, skip
	je read_loop
	jmp *%rax # Jump to previous instruction
.endm

.macro read_repetitions reg_id
1:
	vmovdqu 1(%r14), %ymm4 # Load next 32 chars
	vpxor %ymm4, %ymm\reg_id, %ymm4 # Subtract by mask, now only 1's remain where the char is not the char we are looking for
	
	movq %xmm4, %rax # Get first quad
	cmpq $0, %rax # Check if all zero's, if so we can continue reading
	jne 2f
	addq $8, %r14 # Increment brainfuck source pointer

	vpsrldq $8, %xmm4, %xmm5 # Get second quad
	movq %xmm5, %rax
	cmpq $0, %rax # Check if all zero's, if so we can continue reading
	jne 2f
	addq $8, %r14 # Increment brainfuck source pointer

	vextracti128 $1, %ymm4, %xmm4 # Get third quad
	movq %xmm4, %rax
	cmpq $0, %rax # Check if all zero's, if so we can continue reading
	jne 2f
	addq $8, %r14 # Increment brainfuck source pointer
	
	vpsrldq $8, %xmm4, %xmm4 # Get fourth quad
	movq %xmm4, %rax
	cmpq $0, %rax # Check if all zero's, if so we can continue reading
	jne 2f
	addq $8, %r14 # Increment brainfuck source pointer
	jmp 1b # Loop
2:
	bsfq %rax, %rax # Find first 1
	shrq $3, %rax # Divide by 8 to get the byte of the 1
	addq %rax, %r14 # Increment brainfuck source pointer by the number of bytes that were read
.endm

.align 32
read_repetitions_init_table:
	.quad 0x2B2B2B2B2B2B2B2B, 0x2B2B2B2B2B2B2B2B, 0x2B2B2B2B2B2B2B2B, 0x2B2B2B2B2B2B2B2B # +
	.quad 0x2D2D2D2D2D2D2D2D, 0x2D2D2D2D2D2D2D2D, 0x2D2D2D2D2D2D2D2D, 0x2D2D2D2D2D2D2D2D # -
	.quad 0x3E3E3E3E3E3E3E3E, 0x3E3E3E3E3E3E3E3E, 0x3E3E3E3E3E3E3E3E, 0x3E3E3E3E3E3E3E3E # >
	.quad 0x3C3C3C3C3C3C3C3C, 0x3C3C3C3C3C3C3C3C, 0x3C3C3C3C3C3C3C3C, 0x3C3C3C3C3C3C3C3C # <

brainfuck:
	PROLOGUE

	# Free up %r12-15 and rbx
	push %r12 # -8  Becomes intermediate source write pointer
	push %r13 # -16 Becomes maximum executable memory size
	push %r14 # -24 Becomes brainfuck source pointer
	push %r15 # -32 Becomes last read instruction
	push %rbx # -40 Becomes instruction repetition counter

	# Each bracket frame contains 24 bytes. The first quad contain the previous %rbp. The second quad are used to store 
	# the address of the if statement. The next 4 bytes are used to keep track of memory pointer offset. The last 4 bytes are 
	# used to keep track of some loop optimization possibilities.
	# Push first bracket frame
	pushq %rbp
	movq %rsp, %rbp
	pushq $0
	pushq $0

	# Init %ymm0 - 2 for reading repetitions
	vmovdqa read_repetitions_init_table, %ymm0
	vmovdqa read_repetitions_init_table + 32, %ymm1
	vmovdqa read_repetitions_init_table + 64, %ymm2
	vmovdqa read_repetitions_init_table + 96, %ymm3

	# Init loop variables
	movq $intermediate_src, %r12 # Init intermediate source write pointer
	movq $4, %r13 # Init at 4 for the init of %13 and the ret
	movq %rdi, %r14 # Save brainfuck source pointer
	movq $read_loop, %r15 # Last read instruction
	movq $0, %rbx # The instruction repetition counter

	# Loop through brainfuck source
	decq %r14 # Decrement brainfuck source by one to increment to 0 in loop
	movb $0, %al
read_loop_end_condition:
	cmpb $-43, %al # This check can be skipped when reading most characters. Only if a non-brainfuck char is read
	je compile_char_null # do we have to do this check

read_loop:
	# Get next char
	incq %r14
	movzb (%r14), %rax

	# Index into jump table
	subb $43, %al # Subtract + to narrow the bounds of the chars
	testb $0xC0, %al
	jnz read_loop_end_condition
	shl $3, %rax
	jmp *compile_char_jmp_table(%rax)

compile_char_simple if compile_if 1 # Also contains the labels
compile_char_simple for compile_for 1
compile_char_simple left compile_right -1
compile_char_simple right compile_right 1
compile_char_simple minus compile_plus -1
compile_char_simple plus compile_plus 1
compile_char_simple in compile_in 1
compile_char_simple out compile_out 1

compile_char_null:
	decq %r14 # Decrement src ptr to guarantee that the next loop cicle will also read a null termination character
	compile_char_simple return compile_second_pass 1

compile_char_left_repetition:
	cmpq $compile_right, %r15 # Check if last instruction was a right
	jne compile_char_left # Run last instruction and append a left instruction
	decl %ebx # Decrement instruction repetition counter
	addl %r14d, %ebx # Increment repetition counter by the current pointer
	read_repetitions 3
	subl %r14d, %ebx # Decrement repetition counter by the new pointer
	jmp read_loop # Loop

compile_char_right_repetition:
	cmpq $compile_right, %r15 # Check if last instruction was a right
	jne compile_char_right # Run last instruction and append a right instruction
	incl %ebx # Increment instruction repetition counter
	subl %r14d, %ebx # Decrement repetition counter by the current pointer
	read_repetitions 2
	addl %r14d, %ebx # Increment repetition counter by the new pointer
	jmp read_loop # Loop

compile_char_minus_repetition:
	cmpq $compile_plus, %r15 # Check if last instruction was a plus
	jne compile_char_minus # Run last instruction and append a minus instruction
	decb %bl # Decrement instruction repetition counter
	addb %r14b, %bl # Increment repetition counter by the current pointer
	read_repetitions 1
	subb %r14b, %bl # Decrement repetition counter by the new pointer
	jmp read_loop # Loop

compile_char_plus_repetition:
	cmpq $compile_plus, %r15 # Check if last instruction was a plus
	jne compile_char_plus # Run last instruction and append a plus instruction
	incb %bl # Increment instruction repetition counter
	subb %r14b, %bl # Decrement repetition counter by the current pointer
	read_repetitions 0
	addb %r14b, %bl # Increment repetition counter by the new pointer
	jmp read_loop # Loop

compile_char_jmp_table:
	.quad compile_char_plus_repetition 	# 0 +
	.quad compile_char_in				# 1 ,
	.quad compile_char_minus_repetition # 2 -
	.quad compile_char_out		  # 3 .
	.quad read_loop_end_condition # 4
	.quad read_loop_end_condition # 5
	.quad read_loop_end_condition # 6
	.quad read_loop_end_condition # 7
	.quad read_loop_end_condition # 8
	.quad read_loop_end_condition # 9
	.quad read_loop_end_condition # 10
	.quad read_loop_end_condition # 11
	.quad read_loop_end_condition # 12
	.quad read_loop_end_condition # 13
	.quad read_loop_end_condition # 14
	.quad read_loop_end_condition # 15
	.quad read_loop_end_condition # 16
	.quad compile_char_left_repetition # 17 <
	.quad read_loop_end_condition # 18
	.quad compile_char_right_repetition # 19 >
	.quad read_loop_end_condition # 20
	.quad read_loop_end_condition # 21
	.quad read_loop_end_condition # 22
	.quad read_loop_end_condition # 23
	.quad read_loop_end_condition # 24
	.quad read_loop_end_condition # 25
	.quad read_loop_end_condition # 26
	.quad read_loop_end_condition # 27
	.quad read_loop_end_condition # 28
	.quad read_loop_end_condition # 29
	.quad read_loop_end_condition # 30
	.quad read_loop_end_condition # 31
	.quad read_loop_end_condition # 32
	.quad read_loop_end_condition # 33
	.quad read_loop_end_condition # 34
	.quad read_loop_end_condition # 35
	.quad read_loop_end_condition # 36
	.quad read_loop_end_condition # 37
	.quad read_loop_end_condition # 38
	.quad read_loop_end_condition # 39
	.quad read_loop_end_condition # 40
	.quad read_loop_end_condition # 41
	.quad read_loop_end_condition # 42
	.quad read_loop_end_condition # 43
	.quad read_loop_end_condition # 44
	.quad read_loop_end_condition # 45
	.quad read_loop_end_condition # 46
	.quad read_loop_end_condition # 47
	.quad compile_char_if		  # 48 [
	.quad read_loop_end_condition # 49
	.quad compile_char_for		  # 50 ]
	.quad read_loop_end_condition # 51
	.quad read_loop_end_condition # 52
	.quad read_loop_end_condition # 53
	.quad read_loop_end_condition # 54
	.quad read_loop_end_condition # 55
	.quad read_loop_end_condition # 56
	.quad read_loop_end_condition # 57
	.quad read_loop_end_condition # 58
	.quad read_loop_end_condition # 59
	.quad read_loop_end_condition # 60
	.quad read_loop_end_condition # 61
	.quad read_loop_end_condition # 62
	.quad read_loop_end_condition # 63


##############################################################################################################################################
# Compile
##############################################################################################################################################
.macro write_intermediate_instruction op_code
	movq \op_code, (%r12) # Write op code and clear the rest
	addq $8, %r12 # Increment intermediate write pointer
.endm

.macro write_intermediate_instruction_amount op_code, amount
	write_intermediate_instruction \op_code
	movb \amount, -7(%r12)
.endm

.macro write_intermediate_instruction_offset op_code, offset
	write_intermediate_instruction \op_code
	movl \offset, -6(%r12)
.endm

.macro write_intermediate_instruction_amount_offset op_code, amount, offset
	write_intermediate_instruction_amount \op_code, \amount
	movl \offset, -6(%r12)
.endm

compile_if:
	# Write instruction
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_intermediate_instruction_offset $OP_CODE_IF, %eax
	addq $MAX_INSTRUCTION_SIZE_IF, %r13 # Increment maximum executable memory size

	# Write new bracket frame
	shlq $32, %rax # Move offset to the upper 32 bits
	pushq %rbp
	movq %rsp, %rbp
	pushq %r12 # Push new empty bracket frame
	pushq %rax
	jmp read_loop

compile_for:
	# Get flags and total memory pointer movement in this bracket frame
	movq -16(%rbp), %rax # Get memory pointer offset and flags
	movq %rax, %rdx # Extract memory pointer offset
	shrq $32, %rdx
	movq (%rbp), %r8 # Get parent bracket frame
	subl -12(%r8), %edx # Subtract parent memory pointer offset

	# Test if no optimizations are possible
	testl $LOOP_NO_OPTIMIZATIONS, %eax # Check if no optimizations are possible
	jnz compile_for_no_optimizations

	# Optimizations are possible, jump to appropriate one
	testl %edx, %edx # Check if it is zero
	jz 1f
	orl $LOOP_HAS_TOTAL_RIGHT, %eax # If so, set flag
	# Writing the move instruction to pop the net movement is left to the specific loop compilation, amount passed in %r8d
1:
	shll $3, %eax # Multiply flags by 8 and use as an index into the jump table
	jmp *compile_loop_jmp_table(%eax)


compile_right:
	addl %edx, -12(%rbp) # Increment memory pointer offset in bracket frame
	jmp read_loop

compile_plus:
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_intermediate_instruction_amount_offset $OP_CODE_PLUS, %dl, %eax
	addq $MAX_INSTRUCTION_SIZE_PLUS, %r13 # Increment maximum executable memory size
	orl $LOOP_CONTAINS_PLUS, -16(%rbp) # Loop contains a plus instruction
	jmp read_loop

compile_in:
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_intermediate_instruction_offset $OP_CODE_IN, %eax
	addq $MAX_INSTRUCTION_SIZE_IN, %r13 # Increment maximum executable memory size
	orl $LOOP_CONTAINS_IN, -16(%rbp) # Loop contains a in instruction
	jmp read_loop

compile_out:
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_intermediate_instruction_offset $OP_CODE_OUT, %eax
	addq $MAX_INSTRUCTION_SIZE_OUT, %r13 # Increment maximum executable memory size
	orl $LOOP_CONTAINS_OUT, -16(%rbp) # Loop contains a out instruction
	jmp read_loop


##############################################################################################################################################
# Compile loops
##############################################################################################################################################
.macro pop_bracket_frame
	movl -16(%rbp), %r8d # Get flags
	movq %rbp, %rsp # Pop bracket frame
	popq %rbp
	andl $(LOOP_NO_OPTIMIZATIONS), %r8d # Propagate appropriate flags
	orl %r8d, -16(%rbp) # Inset into parent bracket frame
.endm

################################## None ##################################
compile_for_no_optimizations:
	write_intermediate_instruction_offset $OP_CODE_RIGHT, %edx # Pop the net movement of this bracket frame
	pop_bracket_frame

	movl -12(%rbp), %eax # Get offset from memory pointer
	write_intermediate_instruction_offset $OP_CODE_FOR, %eax
	addq $MAX_INSTRUCTION_SIZE_FOR + MAX_INSTRUCTION_SIZE_RIGHT, %r13 # Increment maximum executable memory size

	orl $LOOP_CONTAINS_LOOP, -16(%rbp) # Loop contains a inner loop
	testl %edx, %edx # Check if net movement is zero
	jz read_loop # If so, continue reading
	orl $LOOP_CONTAINS_SCAN, -16(%rbp) # Loop contains a scan instruction
	jmp read_loop


################################## Mult ##################################
compile_loop_mult:
	# Move back the write pointer
	movq -8(%rbp), %rax # Get address of the first instruction after if, and keep a copy of it in %rax
	movq %r12, %r10 # Preserve a copy of the current write pointer in %r10
	movq %rax, %r12 # Then move it back to the if instruction

	# Pop current bracket frame and get memory pointer offset of parent bracket frame into %rdx
	pop_bracket_frame
	movl -12(%rbp), %edx

	# Check if it is a set zero instruction
	subq %rax, %r10 # Get the amount of instructions written
	cmpq $8, %r10 # If only one instruction is written, it is a set zero instruction
	je compile_loop_set_zero
	cmpq $16, %r10 # If only two instructions are written, it is a copy instruction
	je compile_loop_copy
	addq %rax, %r10 # Restore the old write pointer

	/*
		We know every instuction is 8 bytes long. So if the if instruction gets replaced by the load loop count instruction, then every
		following instruction will replace itself, or a previously read instruction, but never one that was yet to be read. So reading and
		writing in one loop is safe. %rax has the address of the first instruction after the if instruction, and it will be the read 
		pointer through the loop. %rdx will hold the parent memory pointer offset to compare for the source add count.
		%r10 has the old write pointer to use as end condition for the loop. Then, %rcx will keep track of the source add count.
		Finally, %r11 had the address of the if instruction to be used later for inserting the load loop count instruction.
	*/

	# Loop through all the plus instructions
	xorq %rcx, %rcx # Set source add count to 0
	subq $8, %rax # Decrement %rax to be incremented on entry in the loop
	movq %rax, %r11 # Get address of if instruction into %r11
compile_mult_loop:
	# Increment and end condition
	addq $8, %rax
	cmpq %rax, %r10
	je compile_mult_loop_end

	# Read next instruction
	movl 2(%rax), %edi # Get the memory pointer offset of the instruction
	cmpl %edx, %edi # Check if it is a source add instruction
	je compile_mult_loop_source_add
	
	movb 1(%rax), %sil # Get amount of the plus instruction
	write_intermediate_instruction_amount_offset $OP_CODE_MULT_ADD, %sil, %edi
	addq $MAX_INSTRUCTION_SIZE_MULT_ADD, %r13 # Increment maximum executable memory size
	jmp compile_mult_loop

compile_mult_loop_source_add:
	addb 1(%rax), %cl # Increment source add count
	jmp compile_mult_loop

compile_mult_loop_end:
	# Set flag of last mult add instruction
	orq $FLAG_LAST_MULT_ADD, -2(%r12) # Write last mult add flag

	# Write set zero instruction
	write_intermediate_instruction_amount_offset $OP_CODE_SET, $0, %edx

	# Write load loop count instruction
	shlq $16, %rdx # Shift memory pointer offset 2 bytes left
	movb %cl, %dh # Insert source add count
	movb $OP_CODE_LOAD_LOOP_COUNT, %dl # Insert opcode
	movq %rdx, (%r11) # Insert instruction, flags were already cleared by the initial move into %r11d

	addq $MAX_INSTRUCTION_SIZE_SET + MAX_INSTRUCTION_SIZE_LOAD_LOOP_COUNT, %r13 # Increment maximum executable memory size
	orl $LOOP_CONTAINS_SET | LOOP_CONTAINS_MULT, -16(%rbp) # Loop contains a set zero and a multiplication
	jmp read_loop


################################## Set zero ##################################
compile_loop_set_zero:
	# Write set zero
	subq $8, %r12 # Move write pointer back onto the if instruction
	write_intermediate_instruction_amount_offset $OP_CODE_SET, $0, %edx
	addq $MAX_INSTRUCTION_SIZE_SET, %r13 # Increment maximum executable memory size
	orl $LOOP_CONTAINS_SET, -16(%rbp) # Loop contains a set zero
	jmp read_loop


################################## Copy ##################################
compile_loop_copy:
	# Get instructions
	movq (%rax), %r8 # Get first instruction
	movq 8(%rax), %rcx # Get second instruction

	# If %rcx is the source add instruction, then swap them
	cmpl %edx, 10(%rax)
	jne 1f
	xchgq %r8, %rcx
	1:

	# Write load loop count instruction
	subq $8, %r12 # Move write pointer back onto the if instruction
	shrq $8, %r8 # Get source add count
	write_intermediate_instruction_amount_offset $OP_CODE_LOAD_LOOP_COUNT, %r8b, %edx

	# Write mult add instruction
	movb %ch, %al # Get multiplication factor
	shrq $16, %rcx # Get memory pointer offset
	write_intermediate_instruction_amount_offset $OP_CODE_MULT_ADD, %al, %ecx
	orq $FLAG_LAST_MULT_ADD, -2(%r12) # Write last mult add flag

	# Write set zero instruction
	write_intermediate_instruction_amount_offset $OP_CODE_SET, $0, %edx
	
 	# Increment maximum executable memory size and do flags
	addq $MAX_INSTRUCTION_SIZE_LOAD_LOOP_COUNT + MAX_INSTRUCTION_SIZE_MULT_ADD + MAX_INSTRUCTION_SIZE_SET, %r13
	orl $LOOP_CONTAINS_SET | LOOP_CONTAINS_MULT, -16(%rbp) # Loop contains a set zero and a multiplication
	jmp read_loop


################################## Scan ##################################
compile_loop_scan:
	# Check if movement fits in one byte
	movsxb %dl, %eax
	cmpl %eax, %edx
	jne compile_for_no_optimizations

	pop_bracket_frame
	movl -12(%rbp), %eax # Read memory pointer offset of right instruction
	subq $8, %r12 # Go back to the if instruction
	addq $MAX_INSTRUCTION_SIZE_SCAN, %r13 # Increment maximum executable memory size, left3 is the biggest variant of the instruction

	write_intermediate_instruction_amount_offset $OP_CODE_SCAN, %dl, %eax
	orl $LOOP_CONTAINS_SCAN, -16(%rbp)
	jmp read_loop


################################## Registers ##################################
compile_for_registers_nested:
	# For this case we only need to check if the nested loops all have the make register flag.
	# If so, then we can remove them and and make this loop a register loop. Otherwise no optimizations are possible.

	movq -8(%rbp), %rax # Get address after if, used as index
	xorq %rcx, %rcx # Reset inner loop count, amount of if's and for's on the stack
	subq $8, %rax # Decrement to increment in loop entry
	compile_for_registers_nested_loop:
		# Incement and end condition
		addq $8, %rax # Increment index
		cmpq %rax, %r12 # Exit if loop index is current for
		je compile_for_registers_nested_loop_end

		# Check if if has the make register flag, if so, we scan until it's for and store both their addresses for clearing of the
		# make register flag. This side-by-side scan is done because double nested loop have already lost their flag, but are still valid.
		cmpb $OP_CODE_IF, (%rax) # Check if it is an if
		jne compile_for_registers_nested_loop
		testw $FLAG_MAKE_REGISTER_LOOP, 6(%rax) # Check if it has the make register flag
		jz compile_for_no_optimizations # If not, then we can't optimize this loop

		pushq %rax # Push address onto the stack for later
		incq %rcx # Increment address count on the stack

		# Scan until the for
		1:
		addq $8, %rax # Increment index
		cmpb $OP_CODE_FOR, (%rax) # Check if it is a for
		jne 1b
		testw $FLAG_MAKE_REGISTER_LOOP, 6(%rax) # Check if it has the make register flag
		jz 1b # If not, this isn't the matching for, so continue searching
		pushq %rax # Push address onto the stack for later
		incq %rcx # Increment address count on the stack
		jmp compile_for_registers_nested_loop
	
	compile_for_registers_nested_loop_end:
	# We can optimize this loop, so remove all the flags and continue through to compile_for_registers

	1:
	popq %rax # Get address of if or for
	andw $~FLAG_MAKE_REGISTER_LOOP, 6(%rax) # Remove make register flag
	loop 1b
	# Continue to compile_for_registers


compile_for_registers:
	movq -8(%rbp), %rax # Get address after if
	orw $FLAG_MAKE_REGISTER_LOOP, -2(%rax) # Set flag in if

	pop_bracket_frame
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_intermediate_instruction_offset $OP_CODE_FOR, %eax
	orw $FLAG_MAKE_REGISTER_LOOP, -2(%r12) # Set flag in for

	# Increment maximum executable memory size
	addq $MAX_INSTRUCTION_SIZE_FOR + 2 * MAX_REGISTER_COUNT * SIZE_OP_REG_ADDR, %r13

	orl $LOOP_CONTAINS_LOOP, -16(%rbp) # Loop contains a inner loop
	jmp read_loop


	

.equ LOOP_CONTAINS_PLUS, 0x1
.equ LOOP_CONTAINS_SET, 0x2
.equ LOOP_CONTAINS_MULT, 0x4
.equ LOOP_CONTAINS_OUT, 0x8
.equ LOOP_CONTAINS_LOOP, 0x10
.equ LOOP_HAS_TOTAL_RIGHT, 0x20

# No optimizations possible
.equ LOOP_NO_OPTIMIZATIONS, 0x10000000
.equ LOOP_CONTAINS_IN, 0x10000000
.equ LOOP_CONTAINS_SCAN, 0x10000000

compile_loop_jmp_table:
	.quad compile_for_no_optimizations # 0x00
	.quad compile_loop_mult			   # 0x01 Mult or set zero
	.quad compile_for_registers		   # 0x02 Normal loop, but can use registers
	.quad compile_for_registers		   # 0x03
	.quad compile_for_registers		   # 0x04
	.quad compile_for_registers		   # 0x05
	.quad compile_for_registers		   # 0x06
	.quad compile_for_registers		   # 0x07
	.quad compile_for_registers		   # 0x08
	.quad compile_for_registers		   # 0x09
	.quad compile_for_registers		   # 0x0A
	.quad compile_for_registers		   # 0x0B
	.quad compile_for_registers		   # 0x0C
	.quad compile_for_registers		   # 0x0D
	.quad compile_for_registers		   # 0x0E
	.quad compile_for_registers		   # 0x0F
	.quad compile_for_registers_nested # 0x10 Normal nested loop, but can use registers
	.quad compile_for_registers_nested # 0x11
	.quad compile_for_registers_nested # 0x12
	.quad compile_for_registers_nested # 0x13
	.quad compile_for_registers_nested # 0x14
	.quad compile_for_registers_nested # 0x15
	.quad compile_for_registers_nested # 0x16
	.quad compile_for_registers_nested # 0x17
	.quad compile_for_registers_nested # 0x18
	.quad compile_for_registers_nested # 0x19
	.quad compile_for_registers_nested # 0x1A
	.quad compile_for_registers_nested # 0x1B
	.quad compile_for_registers_nested # 0x1C
	.quad compile_for_registers_nested # 0x1D
	.quad compile_for_registers_nested # 0x1E
	.quad compile_for_registers_nested # 0x1F
	.quad compile_loop_scan			   # 0x20 Scan loop
	.quad compile_for_no_optimizations # 0x21
	.quad compile_for_no_optimizations # 0x22
	.quad compile_for_no_optimizations # 0x23
	.quad compile_for_no_optimizations # 0x24
	.quad compile_for_no_optimizations # 0x25
	.quad compile_for_no_optimizations # 0x26
	.quad compile_for_no_optimizations # 0x27
	.quad compile_for_no_optimizations # 0x28
	.quad compile_for_no_optimizations # 0x29
	.quad compile_for_no_optimizations # 0x2A
	.quad compile_for_no_optimizations # 0x2B
	.quad compile_for_no_optimizations # 0x2C
	.quad compile_for_no_optimizations # 0x2D
	.quad compile_for_no_optimizations # 0x2E
	.quad compile_for_no_optimizations # 0x2F
	.quad compile_for_no_optimizations # 0x30
	.quad compile_for_no_optimizations # 0x31
	.quad compile_for_no_optimizations # 0x32
	.quad compile_for_no_optimizations # 0x33
	.quad compile_for_no_optimizations # 0x34
	.quad compile_for_no_optimizations # 0x35
	.quad compile_for_no_optimizations # 0x36
	.quad compile_for_no_optimizations # 0x37
	.quad compile_for_no_optimizations # 0x38
	.quad compile_for_no_optimizations # 0x39
	.quad compile_for_no_optimizations # 0x3A
	.quad compile_for_no_optimizations # 0x3B
	.quad compile_for_no_optimizations # 0x3C
	.quad compile_for_no_optimizations # 0x3D
	.quad compile_for_no_optimizations # 0x3E
	.quad compile_for_no_optimizations # 0x3F


##############################################################################################################################################
# Write singular simple instruction
##############################################################################################################################################
.align 4
reg_modrm_reg_table:
	.long 0x00080000 # cl
	.long 0x00100000 # dl
	.long 0x00300000 # sil
	.long 0x00380000 # dil
	.long 0x00000004 # r8b
	.long 0x00080004 # r9b
	.long 0x00100004 # r10b
	.long 0x00180004 # r11b
	.long 0x00300004 # r14b
	.long 0x00000000 # al

reg_modrm_rm_table:
	.long 0x00010000 # cl
	.long 0x00020000 # dl
	.long 0x00060000 # sil
	.long 0x00070000 # dil
	.long 0x00000001 # r8b
	.long 0x00010001 # r9b
	.long 0x00020001 # r10b
	.long 0x00030001 # r11b
	.long 0x00060001 # r14b
	.long 0x00000000 # al

.equ X86_REG_CL, 0x00
.equ X86_REG_DL, 0x01
.equ X86_REG_SIL, 0x02
.equ X86_REG_DIL, 0x03
.equ X86_REG_R8B, 0x04
.equ X86_REG_R9B, 0x05
.equ X86_REG_R10B, 0x06
.equ X86_REG_R11B, 0x07
.equ X86_REG_R14B, 0x08
.equ X86_REG_AL, 0x09

.equ X86_MOVB_Eb_Gb, 0x88 # Eb_Gb is from reg to reg or reg to addr
.equ X86_MOVB_Gb_Eb, 0x8A # Gb_Eb is from addr to reg
.equ X86_ADDB_Eb_Gb, 0x00
.equ X86_ADDB_Gb_Eb, 0x02
.equ X86_SUBB_Eb_Gb, 0x28
.equ X86_SUBB_Gb_Eb, 0x2A
.equ X86_ANDB_Eb_Gb, 0x20
.equ X86_ANDB_Gb_Eb, 0x22
.equ X86_ORB_Eb_Gb, 0x08
.equ X86_ORB_Gb_Eb, 0x0A
.equ X86_XORB_Eb_Gb, 0x30
.equ X86_XORB_Gb_Eb, 0x32

.equ SIZE_OP_REG_REG, 3
.macro write_op_reg_reg op_code, reg1, reg2, write_offset
	movl \reg1, %r8d # Insert source register
	shll $2, %r8d
	movl reg_modrm_reg_table(%r8d), %r8d
	movl \reg2, %r9d
	shll $2, %r9d # Insert dest register
	orl reg_modrm_rm_table(%r9d), %r8d
	orl $(0x00C00040 | (\op_code << 8)), %r8d # Insert op code and empty prefix and modrmf
	movl %r8d, \write_offset(%r13) # Write instruction
.endm

.equ SIZE_OP_REG_ADDR, 8
# .long 0x24848A41		movb address(%r12), %reg	
.macro write_op_reg_addr op_code, reg, address, write_offset
	movl \reg, %r8d # Get register
	shll $2, %r8d
	movl reg_modrm_reg_table(%r8d), %r8d
	orl $(0x24840041 | (\op_code << 8)), %r8d # Insert instruction
	movl %r8d, \write_offset(%r13) # Write instruction
	movl \address, \write_offset + 4(%r13) # Write address
.endm

.equ SIZE_NEG_REG, 3
# .long 0x00D8F640			negb %reg
.macro write_neg_reg reg, write_offset
	movl \reg, %r8d
	shll $2, %r8d
	movl reg_modrm_rm_table(%r8d), %r8d
	orl $0x00D8F640, %r8d
	movl %r8d, \write_offset(%r13)
.endm

.equ SIZE_TEST_ZERO, 3
# .long 0x00C08440				testb %reg, %reg
.macro write_test_zero reg, write_offset
	movl \reg, %r8d
	shll $2, %r8d
	movl reg_modrm_reg_table(%r8d), %r9d # Get register 1
	orl reg_modrm_rm_table(%r8d), %r9d # Get register 2
	orl $0x00C08440, %r9d # Insert instruction
	movl %r9d, \write_offset(%r13) # Write
.endm


##############################################################################################################################################
# Write instruction
##############################################################################################################################################
.equ MAX_INSTRUCTION_SIZE_RIGHT, INSTRUCTION_SIZE_RIGHT
.equ INSTRUCTION_SIZE_RIGHT, 7
.align 8
byte_code_right:
	.byte 0x41, 0x81, 0xC4, 0x00, 0x00, 0x00, 0x00					#0 addl $amount, %r12
.macro write_instruction_right amount
	movl $0x00C48141, (%r13)
	movl \amount, 3(%r13)
	addq $INSTRUCTION_SIZE_RIGHT, %r13
.endm

.equ MAX_INSTRUCTION_SIZE_IN, INSTRUCTION_SIZE_IN
.equ INSTRUCTION_SIZE_IN, 38
.align 8
byte_code_in:
    .byte 0x48, 0xC7, 0xC0, 0x01, 0x00, 0x00, 0x00			#0  movq $1, %rax
    .byte 0x48, 0x89, 0xC7									#7  movq %rax, %rdi
    .byte 0x48, 0xC7, 0xC6; .long output_buffer				#10 movq $output_buffer, %rsi
    .byte 0x4C, 0x89, 0xEA									#17 movq %r13, %rdx
    .byte 0x0F, 0x05										#20 syscall
	.byte 0x4D, 0x31, 0xED									#22 xorq %r13, %r13

	.byte 0xE8, 0x00, 0x00, 0x00, 0x00						#25 call getchar
	.byte 0x41, 0x88, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00	#30 movb %al, address(%r12)
.macro write_instruction_in address
	# Print everything in the output buffer to this point
	movq $5, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_in, %rsi # Source
	rep movsq
	
	movl $output_buffer, 13(%r13)
	movl \address, 34(%r13)

	# Calculate address offset
	movl $getchar - 30, %eax # Get target address minus index of first byte after call
	subl %r13d, %eax # Get offset from start of executable memory
	movl %eax, 26(%r13) # Insert address offset into instruction
	addq $INSTRUCTION_SIZE_IN, %r13
.endm

.equ MAX_INSTRUCTION_SIZE_OUT, INSTRUCTION_SIZE_OUT_ADDR
.equ INSTRUCTION_SIZE_OUT_ADDR, 18
byte_code_out_addr:
	.byte 0x41, 0x8A, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00		#0  movb address(%r12), %al
	.byte 0x41, 0x88, 0x85; .long output_buffer					#8  movb %al, output_buffer(%r13)
	.byte 0x49, 0xFF, 0xC5										#15 incq %r13
.macro write_instruction_out_addr address
	movl $0x24848A41, (%r13)
	movl \address, 4(%r13)
	movl $0x00858841, 8(%r13)
	movl $output_buffer, 11(%r13)
	movl $0x00C5FF49, 15(%r13)
	addq $INSTRUCTION_SIZE_OUT_ADDR, %r13
.endm

.equ INSTRUCTION_SIZE_OUT_REG, 10
#0 .long 0x00858841					movb %reg, output_buffer(%r13)
#8 .byte 0x49, 0xFF, 0xC5			incq %r13
.macro write_instruction_out_reg reg
	movl \reg, %r8d # Get register
	shll $2, %r8d
	movl reg_modrm_reg_table(%r8d), %r8d
	orl $0x00858841, %r8d # Insert instruction
	movq %r8, (%r13) # Write
	movl $output_buffer, 3(%r13) # Insert output_buffer address
	movl $0x00C5FF49, 7(%r13) # Insert increment
	addq $INSTRUCTION_SIZE_OUT_REG, %r13
.endm

.equ MAX_INSTRUCTION_SIZE_IF, INSTRUCTION_SIZE_IF_ADDR
.equ INSTRUCTION_SIZE_IF_ADDR, 15
#0 .byte 0x41, 0x80, 0xBC, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00		cmpb $0, address(%r12)
#9 .byte 0x0F, 0x84, 0x00, 0x00, 0x00, 0x00							je(long jump) address
.macro write_instruction_if_addr address
	movl $0x24BC8041, (%r13)
	movl \address, 4(%r13)
	movl $0x00840F00, 8(%r13)
	addq $INSTRUCTION_SIZE_IF_ADDR, %r13
.endm

.equ INSTRUCTION_SIZE_IF_REG, 6 + SIZE_TEST_ZERO
# .byte 0x0F,0x85, 0x00, 0x00, 0x00, 0x00			jnz(long jump) address
.macro write_instruction_if_reg reg
	write_test_zero \reg, 0
	movw $0x840F, SIZE_TEST_ZERO(%r13)
	addq $INSTRUCTION_SIZE_IF_REG, %r13
.endm

.equ MAX_INSTRUCTION_SIZE_FOR, INSTRUCTION_SIZE_FOR_ADDR
.equ INSTRUCTION_SIZE_FOR_ADDR, 15
#0 .byte 0x41, 0x80, 0xBC, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00		cmpb $0, address(%r12)
#9 .byte 0x0F, 0x85, 0x00, 0x00, 0x00, 0x00							jne(long jump) address
.macro write_instruction_for_addr address
	movl $0x24BC8041, (%r13)
	movl \address, 4(%r13)
	movl $0x00850F00, 8(%r13)
	addq $INSTRUCTION_SIZE_FOR_ADDR, %r13
.endm

.equ INSTRUCTION_SIZE_FOR_REG, 6 + SIZE_TEST_ZERO
# .byte 0x0F,0x85, 0x00, 0x00, 0x00, 0x00			jnz(long jump) address
.macro write_instruction_for_reg reg
	write_test_zero \reg, 0
	movw $0x850F, SIZE_TEST_ZERO(%r13)
	addq $INSTRUCTION_SIZE_FOR_REG, %r13
.endm

# Writes the jump offset at source to be the offset to dest
.macro write_jump_offset from, static_offset_write, dest
	# Calculate address offset
	movq \dest, %r8 # Get target
	subq \from, %r8 # Subtract source
	movl %r8d, \static_offset_write(\from)
.endm

################################## Arithmetic ##################################
.equ MAX_INSTRUCTION_SIZE_PLUS, INSTRUCTION_SIZE_PLUS_ADDR
.equ INSTRUCTION_SIZE_PLUS_ADDR, 9
# .byte 0x41, 0x80, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00		addb amount, address(%r12)
.macro write_instruction_plus_addr amount, address
	movq $0x24848041, (%r13)
	movl \address, 4(%r13)
	movb \amount, 8(%r13)
	addq $INSTRUCTION_SIZE_PLUS_ADDR, %r13
.endm

.equ INSTRUCTION_SIZE_PLUS_REG, 4
# .long 0x00C08040											addb $amount, %reg
.macro write_instruction_plus_reg amount, reg
	movl \reg, %r8d # Get register
	shll $2, %r8d
	movl reg_modrm_rm_table(%r8d), %r8d
	orl $0x00C08040, %r8d # Insert instruction
	movl %r8d, (%r13)
	movb \amount, 3(%r13) # Insert amount
	addq $INSTRUCTION_SIZE_PLUS_REG, %r13
.endm

.equ MAX_INSTRUCTION_SIZE_SET, INSTRUCTION_SIZE_SET_ADDR
.equ INSTRUCTION_SIZE_SET_ADDR, 9
# .byte 0x41, 0xC6, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00			movb $amount, address(%r12)
.macro write_instruction_set_addr amount address
	movl $0x2484C641, (%r13)
	movl \address, 4(%r13)
	movb \amount, 8(%r13)
	addq $INSTRUCTION_SIZE_SET_ADDR, %r13
.endm

.equ INSTRUCTION_SIZE_SET_REG, 3
.align 2
byte_code_set_reg:
	.word 0xB140 # cl			movb $amount, %reg
	.word 0xB240 # dl
	.word 0xB640 # sil
	.word 0xB740 # dil
	.word 0xB041 # r8b
	.word 0xB141 # r9b
	.word 0xB241 # r10b
	.word 0xB341 # r11b
	.word 0xB641 # r14b
	.word 0xB040 # al
.macro write_instruction_set_reg amount reg
	movl \reg, %r8d
	shll $1, %r8d
	movw byte_code_set_reg(%r8d), %r8w
	movw %r8w, (%r13)
	movb \amount, 3(%r13)
	addq $INSTRUCTION_SIZE_SET_REG, %r13
.endm

################################## Load loop count ##################################
.equ MAX_INSTRUCTION_SIZE_LOAD_LOOP_COUNT, MAX_INSTRUCTION_SIZE_LOAD_LOOP_COUNT_SCAN + SIZE_NEG_REG
.equ INSTRUCTION_SIZE_LOAD_LOOP_COUNT_POW2, 26
.align 8
byte_code_load_loop_count_pow2:
	.byte 0x40, 0xF6, 0xC0, 0x00						#0  testb $test_mask, %reg
	.byte 0x74, 0x10									#4  jz 1f
	.byte 0x48, 0xC7, 0xC0, 0x3C, 0x00, 0x00, 0x00		#6  movq $60, %rax (Exit)
	.byte 0x48, 0xC7, 0xC7, 0x0A, 0x00, 0x00, 0x00		#13 movq $10, %rdi
	.byte 0x0F, 0x05									#20 syscall
	.byte 0x40, 0xC0, 0xE8, 0x00						#22 1: shrb $shift_count, %reg
	.byte 0x00, 0x00, 0x00, 0x00, 0x00, 0x00			# Padding
.macro write_instruction_load_loop_count_pow2 add_count, reg
	movq $4, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_load_loop_count_pow2, %rsi # Source
	rep movsq

	# Insert register
	movl \reg, %r8d	
	shll $2, %r8d
	movl reg_modrm_rm_table(%r8d), %r8d
	orl %r8d, (%r13)
	orl %r8d, 22(%r13)

	# Insert test mask and shift count
	decb \add_count # Get test mask
	movb \add_count, 3(%r13)
	incb \add_count # Restore
	movzb \add_count, %r8w
	bsfw %r8w, %r8w # Get shift count
	movb %r8b, 25(%r13)
	addq $INSTRUCTION_SIZE_LOAD_LOOP_COUNT_POW2, %r13
.endm

.equ MAX_INSTRUCTION_SIZE_LOAD_LOOP_COUNT_SCAN, 36
.align 8
byte_code_load_loop_count_scan:
	.byte 0x40, 0x88, 0xC0						#-4 movb %reg, %al			optional, only when reg is not al
	.byte 0x51									#-1 pushq %rcx				optional, only when reg is al or r14b
	.byte 0x41, 0x80, 0xC6, 0x00				#0  movb $add_count, %r14b
	.byte 0x40, 0x30, 0xC0						#4  xorb %reg, %reg
	.byte 0xB4, 0x00							#7  1: movb $0, %ah
	.byte 0x41, 0xF6, 0xF6						#9  divb %r14b
	.byte 0x40, 0x00, 0xC0						#12 addb %al, %reg
	.byte 0x80, 0xFC, 0x00						#15 cmpb $0, %ah
	.byte 0x74, 0x09							#18 je 1f
	.byte 0x88, 0xE0							#20 movb %ah, %al
	.byte 0x28, 0xC8							#22 subb %r14b, %al
	.byte 0x40, 0xFE, 0xC0						#24 incb %reg
	.byte 0xEB, 0xEA							#27 jmp 1b 1:
	.byte 0x59									#29 popq %rcx				optional, only when reg is al or r14b
	.byte 0x40, 0x88, 0xC0						#30 movb %cl, %reg			optional, only when reg is al or r14b
	.byte 0x00									# Padding
.macro write_instruction_load_loop_count_scan add_count, reg
	# Insert first optional move
	movl \reg, %r8d
	cmpl $X86_REG_AL, %r8d
	je 1f
	write_op_reg_reg X86_MOVB_Eb_Gb, \reg, $X86_REG_AL, 0
	addq $SIZE_OP_REG_REG, %r13
	
	# Insert optional push, pop and second optional move
	movl \reg, %r8d
	cmpl $X86_REG_R14B, %r8d
	jne 2f
	1:
	movb $0x51, (%r13) # Push
	movl $0xC8884059, 30(%r13) # Pop and move
	shll $2, %r8d
	movl reg_modrm_rm_table(%r8d), %r9d
	orl %r9d, 31(%r13) # Insert register

	incq %r13 # Inc length by one
	movq %r13, %rdi # Set destination for the rep movsq
	addq $4, %r13

	movl $0x00080000, %r8d # Overwrite register modrm reg
	movl $0x00010000, %r9d # Overwrite register modrm rm
	jmp 1f

	2:
	movq %r13, %rdi # Set destination for the rep movsq
	movl reg_modrm_rm_table(%r8d), %r9d # Get modrm rm
	movl reg_modrm_reg_table(%r8d), %r8d # Get modrm reg
	1:

	movq $3, %rcx # Quad count, last 5 bytes will be skipped
	movq $byte_code_load_loop_count_scan + 4, %rsi # Source
	rep movsq
	movl $0xEBC0FE40, (%rdi) # Insert last 5 bytes
	movb $0xEA, 4(%rdi)
	movb \add_count, -21(%rdi) # Insert add count
	addq $29, %r13

	# Insert registers
	orl %r8d, -20(%rdi)
	orl %r9d, -20(%rdi)
	orl %r9d, -12(%rdi)
	orl %r9d, -1(%rdi)
.endm

# For load loop count we count down. If the add count is negative, we just negate it and do the
# counting down. If it is positive, we negate the input so that we can count down on that.
.macro write_load_loop_count_addr add_count, address
	write_op_reg_addr X86_MOVB_Gb_Eb, $X86_REG_R14B, \address, 0
	addq $SIZE_OP_REG_ADDR, %r13
	write_load_loop_count_reg \add_count, $X86_REG_R14B
.endm

.macro write_load_loop_count_reg add_count, reg
	# Normalize everything to positive cases
	cmpb $0, \add_count # Check if add count is negative
	jg 10f
	negb \add_count # Negate add count
	jmp 20f
10:
	write_neg_reg \reg
	addq $SIZE_NEG_REG, %r13
20:

	# Check case 1
	cmpb $1, \add_count # If it is, then repetition count is x, which is already done, so we return
	je 30f
	
	# Check case power of 2
	movzb \add_count, %r8
	popcnt %r8, %r8
	cmpb $1, %r8b
	jne 10f
	write_instruction_load_loop_count_pow2 \add_count, \reg
	jmp 30f
10:

	# Not a simplified scan, so just search with a loop
	write_instruction_load_loop_count_scan \add_count, \reg
30:
.endm


################################## Mult add ##################################
.equ INSTRUCTION_SIZE_MULT_ADD_POW2_ADDR, SIZE_MULT_ADD_POW2 + SIZE_OP_REG_ADDR
.macro write_instruction_mult_add_pow2_addr amount, reg_src, address
	write_mult_add_pow2 \amount, \reg_src
	write_op_reg_addr X86_ADDB_Eb_Gb, \reg_src, \address, SIZE_MULT_ADD_POW2
	addq $INSTRUCTION_SIZE_MULT_ADD_POW2_ADDR, %r13
.endm

.equ INSTRUCTION_SIZE_MULT_ADD_POW2_REG, SIZE_MULT_ADD_POW2 + SIZE_OP_REG_REG
.macro write_instruction_mult_add_pow2_reg amount, reg_src, reg_dst
	write_mult_add_pow2 \amount, \reg_src
	write_op_reg_reg X86_ADDB_Eb_Gb, \reg_src, \reg_dst, SIZE_MULT_ADD_POW2
	addq $INSTRUCTION_SIZE_MULT_ADD_POW2_REG, %r13
.endm

.equ SIZE_MULT_ADD_POW2, 4
# .byte 0x40, 0xC0, 0xE0, 0x00				shlb $shift_count, %reg
.macro write_mult_add_pow2 amount, reg
	# Get register
	movl \reg, %r9d
	shll $2, %r9d
	movl reg_modrm_rm_table(%r9d), %r9d
	orl $0x00E0C040, %r9d # Insert instruction
	movl %r9d, (%r13) # Write

	# Get shift count
	movzb \amount, %r8w
	bsfw %r8w, %r8w
	movb %r8b, 3(%r13)
.endm

.macro write_instruction_mult_add_addr amount, reg_src, address
	write_mult_add \amount, \reg_src
	write_op_reg_addr X86_ADDB_Eb_Gb, $X86_REG_AL, \address, 0
	addq $SIZE_OP_REG_ADDR, %r13
.endm

.macro write_instruction_mult_add_reg amount, reg_src, reg_dst
	write_mult_add \amount, \reg_src
	write_op_reg_reg X86_ADDB_Eb_Gb, $X86_REG_AL, \reg_dst, 0
	addq $SIZE_OP_REG_REG, %r13
.endm

# If reg is not al, then:
# .byte 0xB0, 0x00								movb $amount, %al
# .byte 0x40, 0xF6, 0xE0						mulb %reg
# Otherwise:
# .byte 0x41, 0xB6, 0x00						movb $amount, %r14b
# .byte 0x41, 0xF6, 0xE6						mulb %r14b
.macro write_mult_add amount, reg
	movl \reg, %r8d
	cmpl $X86_REG_AL, %r8d # Check if reg is al
	je 1f

	movb $0xB0, (%r13) # movb $amount, %al
	movb \amount, 1(%r13)
	
	shll $2, %r8d # mulb %reg
	movl reg_modrm_rm_table(%r8), %r8d
	orl $0x00E0F640, %r8d
	movl %r8d, 2(%r13)
	addq $5, %r13
	jmp 2f

	1:
	movw $0xB641, (%r13) # movb $amount, %r14b
	movb \amount, 2(%r13)
	movl $0x00E6F641, 3(%r13) # mulb %r14b
	addq $6, %r13
	2:
.endm

.equ MAX_INSTRUCTION_SIZE_MULT_ADD, SIZE_OP_REG_ADDR + SIZE_OP_REG_REG + 6
.macro write_mult_add_addr amount, reg_src, address, flags
	write_mult_add_addr_or_reg addr, \amount, \reg_src, \address, \flags
.endm

.macro write_mult_add_reg amount, reg_src, reg_dst, flags
	write_mult_add_addr_or_reg reg, \amount, \reg_src, \reg_dst, \flags
.endm

.macro write_mult_add_addr_or_reg addr_or_reg, amount, reg_src, dst, flags
	# Get absolute value of amount in %r8
	movb \amount, %r10b
	cmpb $0, %r10b
	jge 10f
	negb %r10b
10:

	# Check if amount is 1
	cmpb $1, %r10b
	jne 10f
	write_op_reg_\addr_or_reg X86_ADDB_Eb_Gb, \reg_src, \dst, 0
	.if \addr_or_reg == "reg"
		addq $SIZE_OP_REG_REG, %r13
	.else
		addq $SIZE_OP_REG_ADDR, %r13
	.endif
	jmp 30f
10:

	# Check if amount is power of 2
	movzb %r10b, %r8
	popcnt %r8, %r9
	cmpb $1, %r9b
	jne 20f

	testw $FLAG_LAST_MULT_ADD, \flags # Test if we need to preserve the loaded loop count
	jz 1f
	movl \reg_src, %edi # Set source to loop count register
	jmp 10f
	1:
	write_op_reg_reg X86_MOVB_Eb_Gb, \reg_src, $X86_REG_AL # Move loop count register to %al
	addq $SIZE_OP_REG_REG, %r13
	movl $X86_REG_AL, %edi # Use %al for multiplication
	10:

	write_instruction_mult_add_pow2_\addr_or_reg %r10b, %edi, \dst
	jmp 30f
20:

	# Not a simplified multiplication, so use a mult insruction
	write_instruction_mult_add_\addr_or_reg \amount, \reg_src, \dst
	jmp 40f

30:
	# Invert additions to subtractions if amount is negative
	cmpb $0, \amount
	jge 40f
	.if \addr_or_reg == "reg"
		movb $0x28, -2(%r13)
	.else
		movb $0x28, -7(%r13)
	.endif
40:
.endm

################################## Scan ##################################
.equ MAX_INSTRUCTION_SIZE_SCAN, INSTRUCTION_SIZE_SCAN_LEFT_3
.equ INSTRUCTION_SIZE_SCAN_LOOP, 25
.align 8
byte_code_scan_loop:
	.byte 0x49, 0x81, 0xEC, 0x00, 0x00, 0x00, 0x00					#0  subq $right_count, %r12
	.byte 0x49, 0x81, 0xC4, 0x00, 0x00, 0x00, 0x00					#7  1: addq $right_count, %r12
	.byte 0x41, 0x80, 0xBC, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00		#14 cmpb $0, address(%r12)
	.byte 0x75, 0xEE												#23 jne 1b
	.byte 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00					# Padding
.macro write_instruction_scan_loop right_count, address
	movsxb \right_count, %r8d # Get right count as a long
	movq $4, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_scan_loop, %rsi # Source
	rep movsq
	movl %r8d, 3(%r13)
	movl %r8d, 10(%r13)
	movl \address, 18(%r13)
	addq $INSTRUCTION_SIZE_SCAN_LOOP, %r13
.endm

.align 32
scan_subtraction_minuends_right:
	.quad 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 # 0
	.quad 0x0101010101010101, 0x0101010101010101, 0x0101010101010101, 0x0101010101010101 # 32  Right 1
	.quad 0x0001000100010001, 0x0001000100010001, 0x0001000100010001, 0x0001000100010001 # 64  Right 2
	.quad 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 # 96
scan_subtraction_minuends_left:
	.quad 0x0000000100000001, 0x0000000100000001, 0x0000000100000001, 0x0000000100000001 # 128 Right 4
	.quad 0x0101010101010101, 0x0101010101010101, 0x0101010101010101, 0x0101010101010101 # 32  Left 1
	.quad 0x0100010001000100, 0x0100010001000100, 0x0100010001000100, 0x0100010001000100 # 64  Left 2
	.quad 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 # 96
	.quad 0x0100000001000000, 0x0100000001000000, 0x0100000001000000, 0x0100000001000000 # 160 Left 4
scan_subtraction_minuends_threes:
	.quad 0x0001000001000001, 0x0100000100000100, 0x0000010000010000, 0x0001000001000001
	.word 0x0100

.equ INSTRUCTION_SIZE_SCAN_RIGHT_3, 120
.align 8
byte_code_scan_right_3:
	.byte 0x48, 0xC7, 0xC1, 0x01, 0x00, 0x00, 0x00						#0   movq $1, %rcx
	.byte 0xFE, 0xC9													#7   1: decb %cl
	.byte 0x79, 0x02													#9   jns 2f
	.byte 0xB1, 0x02													#11  movb $2, %cl

	.byte 0xC5, 0xFE, 0x6F, 0x81, 0x00, 0x00, 0x00, 0x00				#13  2: vmovdqu scan_subtraction_minuends_threes(%rcx), %ymm0
	.byte 0xC4, 0xC1, 0x7E, 0x6F, 0x8C, 0x24, 0x00, 0x00, 0x00, 0x00	#21  vmovdqu address(%r12), %ymm1
	.byte 0xC5, 0xFD, 0xD8, 0xC9, 0x66									#31  vpsubusb %ymm1, %ymm0, %ymm1
	.byte 0x49, 0x0F, 0x7E, 0xC8										#36  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0												#40  testq %r8, %r8
	.byte 0x75, 0x40													#43  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08										#45  addq $8, %r12
	.byte 0xC5, 0xE9, 0x73, 0xD9, 0x08, 0x66							#49  vpsrldq $8, %xmm1, %xmm2
	.byte 0x49, 0x0F, 0x7E, 0xD0										#55  movq %xmm2, %r8
	.byte 0x4D, 0x85, 0xC0												#59  testq %r8, %r8
	.byte 0x75, 0x2D													#62  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08										#64  addq $8, %r12
	.byte 0xC4, 0xE3, 0x7D, 0x39, 0xC9, 0x01, 0x66						#68  vextracti128 $1, %ymm1, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8										#75  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0												#79  testq %r8, %r8
	.byte 0x75, 0x19													#82  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08										#84  addq $8, %r12
	.byte 0xC5, 0xF1, 0x73, 0xD9, 0x08, 0x66							#88  vpsrldq $8, %xmm1, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8										#94  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0												#98  testq %r8, %r8
	.byte 0x75, 0x06													#101 jnz 1f
	.byte 0x49, 0x83, 0xC4, 0x08										#103 addq $8, %r12
	.byte 0xEB, 0x9A													#107 jmp 1b

	.byte 0x4D, 0x0F, 0xBC, 0xC0										#109 1: bsrq %r8, %r8
	.byte 0x49, 0xC1, 0xE8, 0x03										#113 shrq $3, %r8
	.byte 0x4D, 0x01, 0xC4												#117 addq %r8, %r12
.macro write_instruction_scan_right_3 address
	movq $15, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_scan_right_3, %rsi # Source
	rep movsq
	addq $scan_subtraction_minuends_threes, 17(%r13) # Insert minuend address
	movl \address, 27(%r13)
	addq $INSTRUCTION_SIZE_SCAN_RIGHT_3, %r13
.endm

.equ INSTRUCTION_SIZE_SCAN_LEFT_3, 126
.align 8
byte_code_scan_left_3:
	.byte 0x48, 0xC7, 0xC1, 0x04, 0x00, 0x00, 0x00						#0   movq $4, %rcx
	.byte 0x80, 0xE9, 0x02												#7   1: subb $2, %cl
	.byte 0x79, 0x03													#10  jns 2f
	.byte 0x80, 0xC1, 0x03												#12  addb $3, %cl

	.byte 0xC5, 0xFE, 0x6F, 0x81, 0x00, 0x00, 0x00, 0x00				#15  2: vmovdqu scan_subtraction_minuends_threes(%rcx), %ymm0
	.byte 0xC4, 0xC1, 0x7E, 0x6F, 0x8C, 0x24, 0x00, 0x00, 0x00, 0x00	#23  vmovdqu address - 31(%r12), %ymm1
	.byte 0xC5, 0xFD, 0xD8, 0xE1										#33  vpsubusb %ymm1, %ymm0, %ymm4
	.byte 0xC4, 0xE3, 0x7D, 0x39, 0xE2, 0x01							#37  vextracti128 $1, %ymm4, %xmm2
	.byte 0xC5, 0xF1, 0x73, 0xDA, 0x08, 0x66							#43  vpsrldq $8, %xmm2, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8										#49  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0												#53  testq %r8, %r8
	.byte 0x75, 0x35													#56  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08, 0x66									#58  subq $8, %r12
	.byte 0x49, 0x0F, 0x7E, 0xD0										#63  movq %xmm2, %r8
	.byte 0x4D, 0x85, 0xC0												#67  testq %r8, %r8
	.byte 0x75, 0x27													#70  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08										#72  subq $8, %r12
	.byte 0xC5, 0xE1, 0x73, 0xDC, 0x08, 0x66							#76  vpsrldq $8, %xmm4, %xmm3
	.byte 0x49, 0x0F, 0x7E, 0xD8										#82  movq %xmm3, %r8
	.byte 0x4D, 0x85, 0xC0												#86  testq %r8, %r8
	.byte 0x75, 0x14													#89  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08, 0x66									#91  subq $8, %r12
	.byte 0x49, 0x0F, 0x7E, 0xE0										#96  movq %xmm4, %r8
	.byte 0x4D, 0x85, 0xC0												#100 testq %r8, %r8
	.byte 0x75, 0x06													#103 jnz 1f
	.byte 0x49, 0x83, 0xEC, 0x08										#105 subq $8, %r12
	.byte 0xEB, 0x98													#109 jmp 1b

	.byte 0x4D, 0x0F, 0xBD, 0xC0										#111 1: bsrq %r8, %r8
	.byte 0x49, 0xC1, 0xE8, 0x03										#115 shrq $3, %r8
	.byte 0x4D, 0x01, 0xC4												#119 addq %r8, %r12
	.byte 0x49, 0x83, 0xEC, 0x07										#122 subq $7, %r12
	.byte 0x00, 0x00													# Padding
.macro write_instruction_scan_left_3 address
	movq $16, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_scan_left_3, %rsi # Source
	rep movsq
	addq $scan_subtraction_minuends_threes, 19(%r13) # Insert minuend address
	subl $31, \address # Insert address - 31
	movl \address, 29(%r13)
	addq $INSTRUCTION_SIZE_SCAN_LEFT_3, %r13
.endm

.equ INSTRUCTION_SIZE_SCAN_RIGHT_POW2, 111
.align 8
byte_code_scan_right_pow2:
	.byte 0x49, 0xC7, 0xC0, 0x00, 0x00, 0x00, 0x00						#0   movq $minuend_index, %r8
	.byte 0xC4, 0xC1, 0x7D, 0x6F, 0x00									#7   vmovdqa (%r8), %ymm0

	.byte 0xC4, 0xC1, 0x7E, 0x6F, 0x8C, 0x24, 0x00, 0x00, 0x00, 0x00	#12  1: vmovdqu address(%r12), %ymm1
	.byte 0xC5, 0xFD, 0xD8, 0xC9, 0x66									#22  vpsubusb %ymm1, %ymm0, %ymm1
	.byte 0x49, 0x0F, 0x7E, 0xC8										#27  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0												#31  testq %r8, %r8
	.byte 0x75, 0x40													#34  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08										#36  addq $8, %r12
	.byte 0xC5, 0xE9, 0x73, 0xD9, 0x08, 0x66							#40  vpsrldq $8, %xmm1, %xmm2
	.byte 0x49, 0x0F, 0x7E, 0xD0										#46  movq %xmm2, %r8
	.byte 0x4D, 0x85, 0xC0												#50  testq %r8, %r8
	.byte 0x75, 0x2D													#53  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08										#55  addq $8, %r12
	.byte 0xC4, 0xE3, 0x7D, 0x39, 0xC9, 0x01, 0x66						#59  vextracti128 $1, %ymm1, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8										#66  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0												#70  testq %r8, %r8
	.byte 0x75, 0x19													#73  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08										#75  addq $8, %r12
	.byte 0xC5, 0xF1, 0x73, 0xD9, 0x08, 0x66							#79  vpsrldq $8, %xmm1, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8										#85  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0												#89  testq %r8, %r8
	.byte 0x75, 0x06													#92  jnz 1f
	.byte 0x49, 0x83, 0xC4, 0x08										#94  addq $8, %r12
	.byte 0xEB, 0xA8													#98  jmp 1b

	.byte 0x4D, 0x0F, 0xBC, 0xC0										#100 1: bsrq %r8, %r8
	.byte 0x49, 0xC1, 0xE8, 0x03										#104 shrq $3, %r8
	.byte 0x4D, 0x01, 0xC4												#108 addq %r8, %r12
	.byte 0x00															# Padding
.macro write_instruction_scan_right_pow2 right_count, address
	movsxb \right_count, %r8 # Get minuend index
	shlq $5, %r8
	addq $scan_subtraction_minuends_right, %r8
	movq $14, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_scan_right_pow2, %rsi # Source
	rep movsq
	movl %r8d, 3(%r13) # Insert minuend index
	movl \address, 18(%r13) # Insert address
	addq $INSTRUCTION_SIZE_SCAN_RIGHT_POW2, %r13
.endm

.equ INSTRUCTION_SIZE_SCAN_LEFT_POW2, 115
.align 8
byte_code_scan_left_pow2:
	.byte 0x49, 0xC7, 0xC0, 0x00, 0x00, 0x00, 0x00						#0   movq $minuend_index, %r8
	.byte 0xC4, 0xC1, 0x7D, 0x6F, 0x00									#7   vmovdqa (%r8), %ymm0

	.byte  0xC4, 0xC1, 0x7E, 0x6F, 0x8C, 0x24, 0x00, 0x00, 0x00, 0x00	#12  1: vmovdqu address - 31(%r12), %ymm1
	.byte 0xC5, 0xFD, 0xD8, 0xE1										#22  vpsubusb %ymm1, %ymm0, %ymm4
	.byte 0xC4, 0xE3, 0x7D, 0x39, 0xE2, 0x01							#26  vextracti128 $1, %ymm4, %xmm2
	.byte 0xC5, 0xF1, 0x73, 0xDA, 0x08, 0x66							#32  vpsrldq $8, %xmm2, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8										#38  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0												#42  testq %r8, %r8
	.byte 0x75, 0x35													#45  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08, 0x66									#47  subq $8, %r12
	.byte 0x49, 0x0F, 0x7E, 0xD0										#52  movq %xmm2, %r8
	.byte 0x4D, 0x85, 0xC0												#56  testq %r8, %r8
	.byte 0x75, 0x27													#59  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08										#61  subq $8, %r12
	.byte 0xC5, 0xE1, 0x73, 0xDC, 0x08, 0x66							#65  vpsrldq $8, %xmm4, %xmm3
	.byte 0x49, 0x0F, 0x7E, 0xD8										#71  movq %xmm3, %r8
	.byte 0x4D, 0x85, 0xC0												#75  testq %r8, %r8
	.byte 0x75, 0x14													#78  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08, 0x66									#80  subq $8, %r12
	.byte 0x49, 0x0F, 0x7E, 0xE0										#85  movq %xmm4, %r8
	.byte 0x4D, 0x85, 0xC0												#89  testq %r8, %r8
	.byte 0x75, 0x06													#92  jnz 1f
	.byte 0x49, 0x83, 0xEC, 0x08										#94  subq $8, %r12
	.byte 0xEB, 0xA8													#98  jmp 1b

	.byte 0x4D, 0x0F, 0xBD, 0xC0										#100 1: bsrq %r8, %r8
	.byte 0x49, 0xC1, 0xE8, 0x03										#104 shrq $3, %r8
	.byte 0x4D, 0x01, 0xC4												#108 addq %r8, %r12
	.byte 0x49, 0x83, 0xEC, 0x07										#111 subq $7, %r12
	.byte 0x00, 0x00, 0x00, 0x00, 0x00									# Padding
.macro write_instruction_scan_left_pow2 left_count, address
	movsxb \left_count, %r8 # Get minuend index
	shlq $5, %r8
	addq $scan_subtraction_minuends_left, %r8
	movq $15, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_scan_left_pow2, %rsi # Source
	rep movsq
	movl %r8d, 3(%r13) # Insert minuend index
	subl $31, \address # Insert address - 31
	movl \address, 18(%r13)
	addq $INSTRUCTION_SIZE_SCAN_LEFT_POW2, %r13
.endm

.macro write_scan right_count, address
	# Get absolute value
	movb \right_count, %r8b
	cmpb $0, %r8b
	jge 10f
	negb %r8b
10:

	# Case greater than 4
	cmpb $4, %r8b
	jbe 10f
	write_instruction_scan_loop \right_count, \address
	jmp 30f
10:

	# Case 3
	cmpb $3, %r8b
	jne 10f
	cmpb $0, \right_count # Check if right count is negative
	jge 20f
	write_instruction_scan_left_3 \address
	jmp 30f
20:
	write_instruction_scan_right_3 \address
	jmp 30f
10:

	# Case power of 2
	cmpb $0, \right_count # Check if right count is negative
	jge 10f
	write_instruction_scan_left_pow2 %r8b, \address
	jmp 30f
10:
	write_instruction_scan_right_pow2 %r8b, \address
	jmp 30f
30:
.endm


##############################################################################################################################################
# Second pass
##############################################################################################################################################
compile_second_pass:
	# %r12 becomes the intermediate source
	# %r13 becomes the executable source block
	# %r14 contains the variable to use for multiplication
	# The next instruction is passed through %rdx

	# Write exit
	write_intermediate_instruction $OP_CODE_EXIT

	# Pop final bracket frame
	movq %rbp, %rsp
	popq %rbp
	
	# Allocate executable memory
    movq $9, %rax      # Syscall number for mmap
    movq $0, %rdi      # Addr = NULL
    movq %r13, %rsi	   # Set length from %r13
    movq $0x7, %rdx    # Prot = PROT_READ | PROT_WRITE | PROT_EXEC
    movq $0x62, %r10   # Flags = MAP_ANONYMOUS | MAP_PRIVATE | MAP_32BIT
    movq $-1, %r8      # Fd = -1 (no file descriptor)
    movq $0, %r9       # Offset = 0
    syscall
	movq %rax, %r13 # Save executable source block
	pushq %r13 # -48 Save executable source block

	# Write 'xor %r13, %r13' into executable memory as first instruction
	movl $0x00ED314D, (%r13)
	addq $3, %r13 # Increment executable source block

	# Loop through intermediate source
	movq $intermediate_src - 8, %r12 # Init %12 to the start of intermediate source
compile_second_pass_loop:
	# Get next instruction
	addq $8, %r12 # Increment index in intermediate source
	movq (%r12), %rdx

	# Jump into jump tablemake
	movzb %dl, %rax
	shlq $3, %rax
	jmp *sp_compile_jmp_table(%rax)


/*
	Each instruction contains 8 bytes: the first is op code. The next is the amount, regardless of if the instruction requires it.
	The next 4 bytes are the memory pointer offset, regardless of if the instruction requires it. And the final 2 bytes contain
	some flags the instruction might use.
*/
.equ OP_CODE_EXIT, 0
.equ OP_CODE_RIGHT, 1
.equ OP_CODE_SCAN, 2
.equ OP_CODE_IN, 3
.equ OP_CODE_OUT, 4 # Everything from if onward supports registers
.equ OP_CODE_IF, 5
.equ OP_CODE_FOR, 6
.equ OP_CODE_PLUS, 7
.equ OP_CODE_SET, 8
.equ OP_CODE_LOAD_LOOP_COUNT, 9
.equ OP_CODE_MULT_ADD, 10

.equ FLAG_MAKE_REGISTER_LOOP, 0x1 # Only for if and for instructions
.equ FLAG_USE_REGISTER, 0x2
.equ FLAG_LAST_MULT_ADD, 0x4 # If the multiplication is only used once, the load loop count doesn't need to be preserved

.equ MAX_REGISTER_COUNT, 8 # We got cl, dl, dil, sil, r8b, r9b, r10b and r11b, so thats 8 registers

sp_compile_jmp_table:
	.quad sp_compile_exit
	.quad sp_compile_right
	.quad sp_compile_scan
	.quad sp_compile_in
	.quad sp_compile_out
	.quad sp_compile_if
	.quad sp_compile_for
	.quad sp_compile_plus
	.quad sp_compile_set
	.quad sp_compile_load_loop_count
	.quad sp_compile_mult_add


sp_compile_exit:
	# Write ret
	movb $0xC3, (%r13)

	SET_INTERMEDIATE_SRC_SIZE_STAT # Comment out above
	DECOMPILER # Comment out above
	GET_TIME # Comment out above

	jmp execute

sp_compile_right:
	shrq $16, %rdx # Get amount
	write_instruction_right %edx
	jmp compile_second_pass_loop

sp_compile_scan:
	movb %dh, %al # Get amount
	shrq $16, %rdx # Get memory pointer offset
	write_scan %al, %edx
	jmp compile_second_pass_loop

sp_compile_in:
	shrq $16, %rdx # Get amount
	write_instruction_in %edx
	jmp compile_second_pass_loop

sp_compile_out:
	shrq $16, %rdx # Get memory pointer offset
	movq %rdx, %rcx # Get flags
	shrq $32, %rcx
	testw $FLAG_USE_REGISTER, %cx # Test if we need to use a register
	jz 100f
	write_instruction_out_reg %edx
	jmp compile_second_pass_loop
	100:
	write_instruction_out_addr %edx
	jmp compile_second_pass_loop

sp_compile_if:
	# Start with the if, then do the loading if necessary
	shrq $16, %rdx # Get register
	movq %rdx, %rcx # Get flags
	shrq $32, %rcx
	testw $FLAG_USE_REGISTER, %cx
	jz 100f
	write_instruction_if_reg %edx
	jmp 200f
	100:
	write_instruction_if_addr %edx
	200:

	testw $FLAG_MAKE_REGISTER_LOOP, %cx
	jz sp_compile_if_write

	/*
		We scan through the instructions untill either we reach the for statement or we have exhausted all registers. For each instruction 
		we check if the register flag is not set. If it isn't, then we assign that instruction the next register and scan forward to assign 
		every instruction with the same address that register too. %rax will be the outer loop index. %rcx will count the amount of registers used. 
		%r8 will be used as the index for the inner loop. When a register has been assigned, we also write the load instruction to load that register
		and we write the address to the stack so that the for instruction can write the store instructions.
	*/

	# Loop
	movq %r12, %rax # Get read pointer
	xorq %rcx, %rcx # Clear %rcx
	sp_compile_registers_loop:
		# Increment and end condition
		addq $8, %rax

		# Check if the register flag is not set
		testw $FLAG_USE_REGISTER, 6(%rax)
		jnz sp_compile_registers_loop_end_condition

		# Set flag and scan forwards
		movl 2(%rax), %edi # Get address
		movq %rax, %r8 # Use %r8 as the inner loop index
		subq $8, %r8 # Decrement %r8 to be incremented on entry in the loop
		sp_compile_registers_loop_inner:
			# Increment and get instruction
			addq $8, %r8
			movq (%r8), %r9

			# Check if use register flag is not set
			testw $FLAG_USE_REGISTER, 6(%r8)
			jnz 1f

			# Check if instruction supports registers
			cmpb $OP_CODE_OUT, %r9b
			jl sp_compile_registers_loop_inner

			# Check if the address is the same
			shrq $16, %r9
			cmpl %edi, %r9d
			jne 1f

			# Set flag and inset register number
			orw $FLAG_USE_REGISTER, 6(%r8)
			movl %ecx, 2(%r8) # Overwrite the address
			1:

			# End condition of inner loop
			testw $FLAG_MAKE_REGISTER_LOOP, 6(%r8)
			jnz sp_compile_registers_loop_inner_end
			jmp sp_compile_registers_loop_inner
		
		sp_compile_registers_loop_inner_end:
		# Write load
		write_op_reg_addr X86_MOVB_Gb_Eb, %ecx, %edi
		addq $SIZE_OP_REG_ADDR, %r13

		pushq %rdi # Push address of instruction to use in the for instruction
		incb %cl # Increment register count
		cmpb $MAX_REGISTER_COUNT, %cl # Check if we have exhausted all registers
		jae sp_compile_registers_loop_end

		sp_compile_registers_loop_end_condition:
		testw $FLAG_MAKE_REGISTER_LOOP, 6(%rax)
		jz sp_compile_registers_loop
	
	sp_compile_registers_loop_end:
	pushq %rcx # Push register count for for instruction to use

	sp_compile_if_write:
	pushq %r13 # Push address the for has to jump to
	jmp compile_second_pass_loop

sp_compile_for:
	shrq $16, %rdx # Get register
	movq %rdx, %rcx # Get flags
	shrq $32, %rcx
	testw $FLAG_USE_REGISTER, %cx

	# Write for instruction
	jz 100f
	write_instruction_for_reg %edx
	jmp 200f
	100:
	write_instruction_for_addr %edx
	200:

	popq %rax # Get address of first instruction after if instruction
	write_jump_offset %r13, -4, %rax

	testw $FLAG_MAKE_REGISTER_LOOP, %cx
	jnz 100f

	write_jump_offset %rax, -4, %r13
	jmp compile_second_pass_loop

	100:
	popq %rdx # Get amount of registers used

	# Set jump address in if instruction
	shlq $3, %rdx # Multiply by INSTRUCTION_SIZE_LOAD or INSTRUCTION_SIZE_STORE, which are both 8
	subl %edx, %eax # Decrement source by the size of the load instructions
	addl %edx, %r13d # Increment destination by the size of the load instructions
	write_jump_offset %rax, -4, %r13
	subl %edx, %r13d # Restore write pointer
	shrq $3, %rdx # Restore register count

	sp_compile_for_loop:
		decq %rdx
		popq %rax # Get address
		write_op_reg_addr X86_MOVB_Eb_Gb, %edx, %eax # Write store
		addq $SIZE_OP_REG_ADDR, %r13
		test %rdx, %rdx
		jnz sp_compile_for_loop
	jmp compile_second_pass_loop

sp_compile_plus:
	movb %dh, %al # Get amount
	shrq $16, %rdx # Get memory pointer offset or register to use

	movq %rdx, %rcx # Get flags
	shrq $32, %rcx
	testw $FLAG_USE_REGISTER, %cx # Test if we need to use a register
	jz 100f
	write_instruction_plus_reg %al, %edx
	jmp compile_second_pass_loop
	100:
	write_instruction_plus_addr %al, %edx
	jmp compile_second_pass_loop
	
sp_compile_set:
	movb %dh, %al # Get amount
	shrq $16, %rdx # Get memory pointer offset

	movq %rdx, %rcx # Get flags
	shrq $32, %rcx
	testw $FLAG_USE_REGISTER, %cx # Test if we need to use a register
	jz 100f
	write_instruction_set_reg %al, %edx
	jmp compile_second_pass_loop
	100:
	write_instruction_set_addr %al, %edx
	jmp compile_second_pass_loop

sp_compile_load_loop_count:
	movb %dh, %al # Get amount
	shrq $16, %rdx # Get memory pointer offset

	movq %rdx, %rcx # Get flags
	shrq $32, %rcx
	testw $FLAG_USE_REGISTER, %cx # Test if we need to use a register
	jz 100f

	write_load_loop_count_reg %al, %edx
	movl %edx, %r14d
	jmp compile_second_pass_loop
	100:
	write_load_loop_count_addr %al, %edx
	movl $X86_REG_R14B, %r14d
	jmp compile_second_pass_loop

sp_compile_mult_add:
	movb %dh, %al # Get amount
	shrq $16, %rdx # Get memory pointer offset

	movq %rdx, %rcx # Get flags
	shrq $32, %rcx
	testw $FLAG_USE_REGISTER, %cx # Test if we need to use a register
	jz 100f
	write_mult_add_reg %al, %r14d, %edx, %cx
	jmp compile_second_pass_loop
	100:
	write_mult_add_addr %al, %r14d, %edx, %cx
	jmp compile_second_pass_loop


##############################################################################################################################################
# Execute
##############################################################################################################################################
execute:
	# For the run part, %r12 will contain the memory pointer and %r13 will contain the output length counter.
	# Call the executable source block
	movq $runtime_memory, %r12
	movq -48(%rbp), %r13 # Get start of executable source block
	call *%r13

	# Print output
	movq $1, %rax
	movq $1, %rdi
	movq $output_buffer, %rsi
	movq %r13, %rdx
	syscall

	# Restore %r12-15 and %rbx
	movq -8(%rbp), %r12
	movq -16(%rbp), %r13
	movq -24(%rbp), %r14
	movq -32(%rbp), %r15
	movq -40(%rbp), %rbx
	EPILOGUE
