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
	subq -48(%rbp), %r13
	movq %r13, intermediate_src_size
	addq -48(%rbp), %r13
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
	# the address of the if statement. The next 4 bytes are used to keep track of the total memory pointer movement. 
	# The last 4 bytes are used to keep track of some loop optimization possibilities.
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
	movb \op_code, (%r12)
	addq $8, %r12 # Increment intermediate write pointer
.endm

.macro write_intermediate_instruction_amount op_code, amount
	movb \amount, 1(%r12)
	write_intermediate_instruction \op_code
.endm

.macro write_intermediate_instruction_offset op_code, offset
	movl \offset, 2(%r12)
	write_intermediate_instruction \op_code
.endm

.macro write_intermediate_instruction_amount_offset op_code, amount, offset
	movl \offset, 2(%r12)
	write_intermediate_instruction_amount \op_code, \amount
.endm

.macro compile_pop_mem_movement
	movl -12(%rbp), %eax # Get memory pointer offset from bracket frame
	testl %eax, %eax # If memory pointer offset is 0, skip
	jz 1f

	# Memory pointer is not set to zero. That will be done by compile_for if it
	# has been found that the loop cannot be optimized. Same goes for incrementing
	# the maximum size of the executable memory.
	write_intermediate_instruction_offset $OP_CODE_RIGHT, %eax # Write instruction
1:
.endm

compile_if:
	compile_pop_mem_movement

	# Write instruction
	movw $0, 6(%r12) # Set flags to 0
	write_intermediate_instruction $OP_CODE_IF
	addq $INSTRUCTION_SIZE_IF, %r13 # Increment maximum executable memory size

	# Write new bracket frame
	pushq %rbp
	movq %rsp, %rbp
	pushq %r12 # Push new empty bracket frame
	pushq $0
	jmp read_loop

compile_for:
	movq -16(%rbp), %rax # Get total memory movement and flags
	movq %rax, %rdx # Extract total memory movement
	shrq $32, %rdx
	cmpl $0, %edx # Check if it is zero
	je 1f
	orl $LOOP_HAS_TOTAL_RIGHT, %eax # If so, set flag
1:
	shll $3, %eax # Multiply flags by 8 and use as an index into the jump table
	jmp *compile_loop_jmp_table(%eax)


compile_right:
	addl %edx, -12(%rbp) # Increment memory pointer offset in bracket frame
	jmp read_loop

compile_plus:
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_intermediate_instruction_amount_offset $OP_CODE_PLUS, %dl, %eax
	addq $INSTRUCTION_SIZE_PLUS, %r13 # Increment maximum executable memory size
	orl $LOOP_CONTAINS_PLUS, -16(%rbp) # Loop contains a plus instruction
	jmp read_loop

compile_in:
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_intermediate_instruction_offset $OP_CODE_IN, %eax
	addq $INSTRUCTION_SIZE_IN, %r13 # Increment maximum executable memory size
	orl $LOOP_CONTAINS_IO, -16(%rbp) # Loop contains a in instruction
	jmp read_loop

compile_out:
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_intermediate_instruction_offset $OP_CODE_OUT, %eax
	addq $INSTRUCTION_SIZE_OUT, %r13 # Increment maximum executable memory size
	orl $LOOP_CONTAINS_IO, -16(%rbp) # Loop contains a out instruction
	jmp read_loop


##############################################################################################################################################
# Compile loops
##############################################################################################################################################
.macro move_before_if_instruction src_pointer
	subq $8, \src_pointer # Move pointer to the if instruction
	cmpb $OP_CODE_RIGHT, -8(\src_pointer) # Check if the previous instruction is a right instruction
	jne 1f
	subq $8, \src_pointer # If so, move to it aswell
1:
.endm

.macro pop_bracket_frame
	movl -16(%rbp), %r8d # Get flags
	movq %rbp, %rsp # Pop bracket frame
	popq %rbp
	andl $(LOOP_CONTAINS_SCAN), %r8d # Propagate appropriate flags
	orl %r8d, -16(%rbp) # Inset into parent bracket frame
.endm

################################## None ##################################
compile_for_no_optimizations:
	compile_pop_mem_movement

	write_intermediate_instruction $OP_CODE_FOR
	pop_bracket_frame
	movl $0, -12(%rbp) # Reset memory pointer to match the movement before the if
	addq $INSTRUCTION_SIZE_FOR + 2 * INSTRUCTION_SIZE_RIGHT, %r13 # Increment maximum executable memory size

	orl $LOOP_CONTAINS_LOOP, -16(%rbp) # Loop contains a inner loop
	jmp read_loop


################################## Mult ##################################
compile_loop_mult:
	# Move back the write pointer
	movq -8(%rbp), %rax # Get address of the first instruction after if, and keep a copy of it in %rax
	movq %r12, %r10 # Preserve a copy of the current write pointer in %r10
	movq %rax, %r12 # Then move it back to the if instruction or the move instruction before the if if present
	move_before_if_instruction %r12

	# Pop current bracket frame and get memory pointer offset of parent bracket frame into %rdx
	pop_bracket_frame
	movl -12(%rbp), %edx

	# Check if it is a set zero instruction
	subq %rax, %r10 # Get the amount of instructions written
	cmpq $8, %r10 # If only one instruction is written, it is a set zero instruction
	je compile_loop_set_zero
	addq %rax, %r10 # Restore the old write pointer

	/*
		We know every instuction is 8 bytes long. So if the if instruction gets replaced by the load loop count instruction, then every
		following instruction will replace itself, or a previously read instruction, but never one that was yet to be read. So reading and
		writing in one loop is safe. %rax has the address of the first instruction after the if instruction, it be the read pointer through 
		the loop. %rdx has the memory pointer offset of the parent bracket frame. %r10 has the old write pointer to use as end condition 
		for the loop. %r11 will hold the location where the load loop count instruction is to be inserted. Finally, %rcx will keep track 
		of the source add count.
	*/

	# Loop through all the plus instructions
	movb $0, %cl # Set source add count to 0
	subq $8, %rax # Decrement %rax to be incremented on entry in the loop
	movq %r12, %r11 # Set %r11 to the location where the load loop count instruction is to be inserted
	addq $8, %r12 # Increment %r12 to make space for the load loop count instruction
compile_mult_loop:
	# Increment and end condition
	addq $8, %rax
	cmpq %rax, %r10
	je compile_mult_loop_end

	# Read next instruction
	movl 2(%rax), %edi # Get the memory pointer offset of the instruction
	testl %edi, %edi # Check if it is a source add instruction
	jz compile_mult_loop_source_add
	
	movb 1(%rax), %sil # Get amount of the plus instruction
	addl %edx, %edi # Add the memory pointer offset of the parent bracket frame
	write_intermediate_instruction_amount_offset $OP_CODE_MULT_ADD, %sil, %edi
	addq $INSTRUCTION_SIZE_MULT_ADD_POW2, %r13 # Increment maximum executable memory size, pow2 is the biggest variant of the instruction
	jmp compile_mult_loop

compile_mult_loop_source_add:
	addb 1(%rax), %cl # Increment source add count
	jmp compile_mult_loop

compile_mult_loop_end:
	# Write set zero instruction
	write_intermediate_instruction_amount_offset $OP_CODE_SET, $0, %edx

	# Write load loop count instruction
	shlq $16, %rdx # Shift memory pointer offset 2 bytes left
	movb %cl, %dh # Insert source add count
	movb $OP_CODE_LOAD_LOOP_COUNT, %dl # Insert opcode
	movq %rdx, (%r11) # Insert instruction

 	# Increment maximum executable memory size, load loop count scan is the biggest variant of the instruction
	addq $INSTRUCTION_SIZE_SET + INSTRUCTION_SIZE_LOAD_LOOP_COUNT_SCAN, %r13
	orl $LOOP_CONTAINS_SET | LOOP_CONTAINS_MULT, -16(%rbp) # Loop contains a set zero and a multiplication
	jmp read_loop


################################## Set zero ##################################
compile_loop_set_zero:
	# Write set zero
	addl 2(%rax), %edx # Take parent bracket memory pointer offset and add to it the offset from this plus instruction to get the offset to write
	write_intermediate_instruction_amount_offset $OP_CODE_SET, $0, %edx
	addq $INSTRUCTION_SIZE_SET, %r13 # Increment maximum executable memory size
	orl $LOOP_CONTAINS_SET, -16(%rbp) # Loop contains a set zero
	jmp read_loop




// ################################## Scan ##################################
compile_loop_scan:	
	movl -12(%rbp), %eax # Read memory pointer offset of right instruction
	subq $8, %r12 # Go back to the if instruction
	pop_bracket_frame
	movl $0, -12(%rbp) # Reset memory pointer to match the movement before the if
	addq $INSTRUCTION_SIZE_RIGHT + INSTRUCTION_SIZE_SCAN_LEFT_3, %r13 # Increment maximum executable memory size, left3 is the biggest variant of the instruction

	write_intermediate_instruction_offset $OP_CODE_SCAN, %eax
	orl $LOOP_CONTAINS_SCAN, -16(%rbp)
	jmp read_loop



.equ LOOP_CONTAINS_PLUS, 0x1
.equ LOOP_CONTAINS_IO, 0x2
.equ LOOP_CONTAINS_SET, 0x4
.equ LOOP_CONTAINS_MULT, 0x8
.equ LOOP_CONTAINS_LOOP, 0x10
.equ LOOP_HAS_TOTAL_RIGHT, 0x20
.equ LOOP_CONTAINS_SCAN, 0x40

compile_loop_jmp_table:
	.quad compile_for_no_optimizations # 0x00
	.quad compile_loop_mult			   # 0x01 Mult or set zero
	.quad compile_for_no_optimizations # 0x02
	.quad compile_for_no_optimizations # 0x03
	.quad compile_for_no_optimizations # 0x04
	.quad compile_for_no_optimizations # 0x05
	.quad compile_for_no_optimizations # 0x06
	.quad compile_for_no_optimizations # 0x07
	.quad compile_for_no_optimizations # 0x08
	.quad compile_for_no_optimizations # 0x09
	.quad compile_for_no_optimizations # 0x0A
	.quad compile_for_no_optimizations # 0x0B
	.quad compile_for_no_optimizations # 0x0C
	.quad compile_for_no_optimizations # 0x0D
	.quad compile_for_no_optimizations # 0x0E
	.quad compile_for_no_optimizations # 0x0F
	.quad compile_for_no_optimizations # 0x10
	.quad compile_for_no_optimizations # 0x11
	.quad compile_for_no_optimizations # 0x12
	.quad compile_for_no_optimizations # 0x13
	.quad compile_for_no_optimizations # 0x14
	.quad compile_for_no_optimizations # 0x15
	.quad compile_for_no_optimizations # 0x16
	.quad compile_for_no_optimizations # 0x17
	.quad compile_for_no_optimizations # 0x18
	.quad compile_for_no_optimizations # 0x19
	.quad compile_for_no_optimizations # 0x1A
	.quad compile_for_no_optimizations # 0x1B
	.quad compile_for_no_optimizations # 0x1C
	.quad compile_for_no_optimizations # 0x1D
	.quad compile_for_no_optimizations # 0x1E
	.quad compile_for_no_optimizations # 0x1F
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
	.quad compile_for_no_optimizations # 0x40
	.quad compile_for_no_optimizations # 0x41
	.quad compile_for_no_optimizations # 0x42
	.quad compile_for_no_optimizations # 0x43
	.quad compile_for_no_optimizations # 0x44
	.quad compile_for_no_optimizations # 0x45
	.quad compile_for_no_optimizations # 0x46
	.quad compile_for_no_optimizations # 0x47
	.quad compile_for_no_optimizations # 0x48
	.quad compile_for_no_optimizations # 0x49
	.quad compile_for_no_optimizations # 0x4A
	.quad compile_for_no_optimizations # 0x4B
	.quad compile_for_no_optimizations # 0x4C
	.quad compile_for_no_optimizations # 0x4D
	.quad compile_for_no_optimizations # 0x4E
	.quad compile_for_no_optimizations # 0x4F
	.quad compile_for_no_optimizations # 0x50
	.quad compile_for_no_optimizations # 0x51
	.quad compile_for_no_optimizations # 0x52
	.quad compile_for_no_optimizations # 0x53
	.quad compile_for_no_optimizations # 0x54
	.quad compile_for_no_optimizations # 0x55
	.quad compile_for_no_optimizations # 0x56
	.quad compile_for_no_optimizations # 0x57
	.quad compile_for_no_optimizations # 0x58
	.quad compile_for_no_optimizations # 0x59
	.quad compile_for_no_optimizations # 0x5A
	.quad compile_for_no_optimizations # 0x5B
	.quad compile_for_no_optimizations # 0x5C
	.quad compile_for_no_optimizations # 0x5D
	.quad compile_for_no_optimizations # 0x5E
	.quad compile_for_no_optimizations # 0x5F
	.quad compile_for_no_optimizations # 0x60
	.quad compile_for_no_optimizations # 0x61
	.quad compile_for_no_optimizations # 0x62
	.quad compile_for_no_optimizations # 0x63
	.quad compile_for_no_optimizations # 0x64
	.quad compile_for_no_optimizations # 0x65
	.quad compile_for_no_optimizations # 0x66
	.quad compile_for_no_optimizations # 0x67
	.quad compile_for_no_optimizations # 0x68
	.quad compile_for_no_optimizations # 0x69
	.quad compile_for_no_optimizations # 0x6A
	.quad compile_for_no_optimizations # 0x6B
	.quad compile_for_no_optimizations # 0x6C
	.quad compile_for_no_optimizations # 0x6D
	.quad compile_for_no_optimizations # 0x6E
	.quad compile_for_no_optimizations # 0x6F
	.quad compile_for_no_optimizations # 0x70
	.quad compile_for_no_optimizations # 0x71
	.quad compile_for_no_optimizations # 0x72
	.quad compile_for_no_optimizations # 0x73
	.quad compile_for_no_optimizations # 0x74
	.quad compile_for_no_optimizations # 0x75
	.quad compile_for_no_optimizations # 0x76
	.quad compile_for_no_optimizations # 0x77
	.quad compile_for_no_optimizations # 0x78
	.quad compile_for_no_optimizations # 0x79
	.quad compile_for_no_optimizations # 0x7A
	.quad compile_for_no_optimizations # 0x7B
	.quad compile_for_no_optimizations # 0x7C
	.quad compile_for_no_optimizations # 0x7D
	.quad compile_for_no_optimizations # 0x7E
	.quad compile_for_no_optimizations # 0x7F


##############################################################################################################################################
# Write instruction
##############################################################################################################################################
.equ INSTRUCTION_SIZE_RIGHT, 7
.align 8
byte_code_right:
	.byte 0x41, 0x81, 0xC4, 0x00, 0x00, 0x00, 0x00					#0 addl $amount, %r12
.macro write_instruction_right amount
	movl $0x00C48141, (%r13)
	movl \amount, 3(%r13)
	addq $INSTRUCTION_SIZE_RIGHT, %r13
.endm

.equ INSTRUCTION_SIZE_PLUS, 9
.align 8
byte_code_plus:
	.byte 0x41, 0x80, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00		#0 addq amount, address(%r12)
.macro write_instruction_plus amount, address
	movq $0x24848041, (%r13)
	movl \address, 4(%r13)
	movb \amount, 8(%r13)
	addq $INSTRUCTION_SIZE_PLUS, %r13
.endm

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

.equ INSTRUCTION_SIZE_OUT, 18
.align 8
byte_code_out:
	.byte 0x41, 0x8A, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00		#0  movb address(%r12), %al
	.byte 0x41, 0x88, 0x85; .long output_buffer					#8  movb %al, output_buffer(%r13)
	.byte 0x49, 0xFF, 0xC5										#15 incq %r13
.macro write_instruction_out address
	movl $0x24848A41, (%r13)
	movl \address, 4(%r13)
	movl $0x00858841, 8(%r13)
	movl $output_buffer, 11(%r13)
	movl $0x00C5FF49, 15(%r13)
	addq $INSTRUCTION_SIZE_OUT, %r13
.endm

.equ INSTRUCTION_SIZE_IF, 11
.align 8
byte_code_if:
	.byte 0x41, 0x80, 0x3C, 0x24, 0x00					#0 cmpb $0, (%r12)
	.byte 0x0F, 0x84, 0x00, 0x00, 0x00, 0x00			#5 je(long jump) address
.macro write_instruction_if
	movl $0x243C8041, (%r13)
	movl $0x00840F00, 4(%r13)
	addq $INSTRUCTION_SIZE_IF, %r13
.endm

.equ INSTRUCTION_SIZE_FOR, 11
.align 8
byte_code_for:
	.byte 0x41,0x80, 0x3C, 0x24, 0x00					#0 cmpb $0, (%r12)
	.byte 0x0F,0x85, 0x00, 0x00, 0x00, 0x00				#5 jne(long jump) address
.macro write_instruction_for
	movl $0x243C8041, (%r13)
	movl $0x00850F00, 4(%r13)
	addq $INSTRUCTION_SIZE_FOR, %r13
.endm

.equ INSTRUCTION_SIZE_SET, 9
.align 8
byte_code_set:
	.byte 0x41, 0xC6, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00			#0 movb $amount, address(%r12)
.macro write_instruction_set amount address
	movl $0x2484C641, (%r13)
	movl \address, 4(%r13)
	movb \amount, 8(%r13)
	addq $INSTRUCTION_SIZE_SET, %r13
.endm


# Writes the jump offset at source to be the offset to dest
.macro write_jump_offset from, static_offset_write, dest
	# Calculate address offset
	movq \dest, %rax # Get target
	subq \from, %rax # Subtract source
	movl %eax, \static_offset_write(\from)
.endm

################################## Load loop count ##################################
.equ INSTRUCTION_SIZE_LOAD, 8
.align 8
byte_code_load:
	.byte 0x45, 0x8A, 0xB4, 0x24, 0x00, 0x00, 0x00, 0x00				#0 movb address(%r12), %r14b
.macro write_instruction_load address
	movl $0x24B48A45, (%r13)
	movl \address, 4(%r13)
	addq $INSTRUCTION_SIZE_LOAD, %r13
.endm

.equ INSTRUCTION_SIZE_NEG_LOADED, 3
.align 8
byte_code_load_neg_loaded:
	.byte 0x41, 0xF6, 0xDE								#0 negb %r14b
.macro write_instruction_neg_loaded address
	movl $0x00DEF641, 8(%r13)
	addq $INSTRUCTION_SIZE_NEG_LOADED, %r13
.endm

.equ INSTRUCTION_SIZE_LOAD_POW2, 26
.align 8
byte_code_load_pow2:
	.byte 0x41, 0xF6, 0xC6, 0x00						#0  testb $test_mask, %r14b
	.byte 0x74, 0x10									#4  jz 1f
	.byte 0x48, 0xC7, 0xC0, 0x3C, 0x00, 0x00, 0x00		#6  movq $60, %rax (Exit)
	.byte 0x48, 0xC7, 0xC7, 0x0A, 0x00, 0x00, 0x00		#13 movq $10, %rdi
	.byte 0x0F, 0x05									#20 syscall
	.byte 0x41, 0xC0, 0xEE, 0x00						#22 1: shrb $shift_count, %r14b
	.byte 0x00, 0x00, 0x00, 0x00, 0x00, 0x00			# Padding
.macro write_instruction_load_pow2 add_count
	movq $4, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_load_pow2, %rsi # Source
	rep movsq

	decb \add_count # Get test mask
	movb \add_count, 3(%r13)
	incb \add_count # Restore
	movzb \add_count, %r8w
	bsfw %r8w, %r8w # Get shift count
	movb %r8b, 25(%r13)
	addq $INSTRUCTION_SIZE_LOAD_POW2, %r13
.endm

.equ INSTRUCTION_SIZE_LOAD_LOOP_COUNT_SCAN, 29
.align 8
byte_code_load_loop_count_scan:
	.byte 0x44, 0x88, 0xF0						#0  movb %r14b, %al
	.byte 0xB1, 0x00							#3  movb $add_count, %cl
	.byte 0x45, 0x30, 0xF6						#5  xorb %r14b, %r14b
	.byte 0xB4, 0x00							#8  1: movb $0, %ah
	.byte 0xF6, 0xF1							#10 divb %cl
	.byte 0x41, 0x00, 0xC6						#12 addb %al, %r14b
	.byte 0x80, 0xFC, 0x00						#15 cmpb $0, %ah
	.byte 0x74, 0x09							#18 je 1f
	.byte 0x88, 0xE0							#20 movb %ah, %al
	.byte 0x28, 0xC8							#22 subb %cl, %al
	.byte 0x41, 0xFE, 0xC6						#24 incb %r14b
	.byte 0xEB, 0xEB							#27 jmp 1b 1:
	.byte 0x00, 0x00, 0x00						# Padding
.macro write_instruction_load_loop_count_scan add_count
	movq $4, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_load_loop_count_scan, %rsi # Source
	rep movsq
	movb \add_count, 4(%r13)
	addq $INSTRUCTION_SIZE_LOAD_LOOP_COUNT_SCAN, %r13
.endm

# For load loop count we count down. If the add count is negative, we just negate it and do the
# counting down. If it is positive, we negate the input so that we can count down on that.
.macro write_load_loop_count add_count, address
	# Load input
	write_instruction_load \address

	# Normalize everything to positive cases
	cmpb $0, \add_count # Check if add count is negative
	jg 1f
	negb \add_count # Negate add count
	jmp 2f
1:
	write_instruction_neg_loaded \address # Negate input
	addq $INSTRUCTION_SIZE_NEG_LOADED, %r13
2:

	# Check case 1
	cmpb $1, \add_count # If it is, then repetition count is x, which is already done, so we return
	je 3f
	
	# Check case power of 2
	movzb \add_count, %r8
	popcnt %r8, %r8
	cmpb $1, %r8b
	jne 1f
	write_instruction_load_pow2 \add_count
	jmp 3f
1:

	# Not a simplified scan, so just search with a loop
	write_instruction_load_loop_count_scan \add_count
3:
.endm


################################## Mult add ##################################
.equ INSTRUCTION_SIZE_ADD, 8
.align 8
byte_code_add:
	.byte 0x45, 0x00, 0xB4, 0x24, 0x00, 0x00, 0x00, 0x00	#0 addb %r14b, address(%r12)
.macro write_instruction_add address
	movl $0x24B40045, (%r13)
	movl \address, 4(%r13)
	addq $INSTRUCTION_SIZE_ADD, %r13
.endm

.equ INSTRUCTION_SIZE_MULT_ADD_POW2, 14
.align 8
byte_code_mult_add_pow2:
	.byte 0x44, 0x88, 0xF0									#0 movb %r14b, %al
	.byte 0xC0, 0xE0, 0x00									#3 shlb $shift_count, %al
	.byte 0x41, 0x00, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00	#6 addb %al, address(%r12)
.macro write_instruction_mult_add_pow2 amount, address
	movzb \amount, %r8w
	bsfw %r8w, %r8w # Get shift count
	movl $0xC0F08844, (%r13)
	movb $0xE0, 4(%r13)
	movb %r8b, 5(%r13)
	movl $0x24840041, 6(%r13)
	movl \address, 10(%r13)
	addq $INSTRUCTION_SIZE_MULT_ADD_POW2, %r13
.endm

.equ INSTRUCTION_SIZE_MULT_ADD, 13
.align 8
byte_code_mult_add:
	.byte 0xB0, 0x00										#0 movb $amount, %al
	.byte 0x41, 0xF6, 0xE6									#2 mulb %r14b
	.byte 0x41, 0x00, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00	#5 addb %al, address(%r12)
.macro write_instruction_mult_add amount, address
	movl $0xF64100B0, (%r13)
	movb \amount, 1(%r13)
	movb $0xE6, 4(%r13)
	movl $0x24840041, 5(%r13)
	movl \address, 9(%r13)
	addq $INSTRUCTION_SIZE_MULT_ADD, %r13
.endm

.macro write_mult_add amount, address
	# Get absolute value of amount in %r8
	movb \amount, %r8b
	cmpb $0, %r8b
	jge 1f
	negb %r8b
1:

	# Check if amount is 1
	cmpb $1, %r8b
	jne 1f
	write_instruction_add \address
	jmp 2f
1:

	# Check if amount is power of 2
	movzb %r8b, %r8
	popcnt %r8, %r9
	cmpb $1, %r9b
	jne 1f
	write_instruction_mult_add_pow2 %r8b \address
	jmp 2f
1:

	# Not a simplified multiplication, so use a mult insruction
	write_instruction_mult_add \amount \address
	jmp 3f

2:
	# Invert additions to subtractions if amount is negative
	cmpb $0, \amount
	jge 3f
	movb $0x28, -7(%r13) # Make addition into a subtraction
3:
.endm

################################## Scan ##################################
.equ INSTRUCTION_SIZE_SCAN_LOOP, 21
.align 8
byte_code_scan_loop:
	.byte 0x49, 0x81, 0xEC, 0x00, 0x00, 0x00, 0x00		#0  subq $right_count, %r12
	.byte 0x49, 0x81, 0xC4, 0x00, 0x00, 0x00, 0x00		#7  1: addq $right_count, %r12
	.byte 0x41, 0x80, 0x3C, 0x24, 0x00					#14 cmpb $0, (%r12)
	.byte 0x75, 0xF2									#19 jne 1b
	.byte 0x00, 0x00, 0x00								# Padding
.macro write_instruction_scan_loop right_count
	movq $3, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_scan_loop, %rsi # Source
	rep movsq
	movl \right_count, 3(%r13)
	movl \right_count, 10(%r13)
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

.equ INSTRUCTION_SIZE_SCAN_RIGHT_3, 116
.align 8
byte_code_scan_right_3:
	.byte 0x48, 0xC7, 0xC1, 0x01, 0x00, 0x00, 0x00			#0   movq $1, %rcx
	.byte 0xFE, 0xC9										#7   1: decb %cl
	.byte 0x79, 0x02										#9   jns 2f
	.byte 0xB1, 0x02										#11  movb $2, %cl

	.byte 0xC5, 0xFE, 0x6F, 0x81, 0x00, 0x00, 0x00, 0x00	#13  2: vmovdqu scan_subtraction_minuends_threes(%rcx), %ymm0
	.byte 0xC4, 0xC1, 0x7E, 0x6F, 0x0C, 0x24				#21  vmovdqu (%r12), %ymm1
	.byte 0xC5, 0xFD, 0xD8, 0xC9, 0x66						#27  vpsubusb %ymm1, %ymm0, %ymm1
	.byte 0x49, 0x0F, 0x7E, 0xC8							#32  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0									#36  testq %r8, %r8
	.byte 0x75, 0x40										#39  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08							#41  addq $8, %r12
	.byte 0xC5, 0xE9, 0x73, 0xD9, 0x08, 0x66				#45  vpsrldq $8, %xmm1, %xmm2
	.byte 0x49, 0x0F, 0x7E, 0xD0							#51  movq %xmm2, %r8
	.byte 0x4D, 0x85, 0xC0									#55  testq %r8, %r8
	.byte 0x75, 0x2D										#58  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08							#60  addq $8, %r12
	.byte 0xC4, 0xE3, 0x7D, 0x39, 0xC9, 0x01, 0x66			#64  vextracti128 $1, %ymm1, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8							#71  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0									#75  testq %r8, %r8
	.byte 0x75, 0x19										#78  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08							#80  addq $8, %r12
	.byte 0xC5, 0xF1, 0x73, 0xD9, 0x08, 0x66				#84  vpsrldq $8, %xmm1, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8							#90  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0									#94  testq %r8, %r8
	.byte 0x75, 0x06										#97  jnz 1f
	.byte 0x49, 0x83, 0xC4, 0x08							#99  addq $8, %r12
	.byte 0xEB, 0x9E										#103 jmp 1b

	.byte 0x4D, 0x0F, 0xBC, 0xC0							#105 1: bsrq %r8, %r8
	.byte 0x49, 0xC1, 0xE8, 0x03							#109 shrq $3, %r8
	.byte 0x4D, 0x01, 0xC4									#113 addq %r8, %r12
	.byte 0x00, 0x00, 0x00, 0x00							# Padding
.macro write_instruction_scan_right_3
	movq $15, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_scan_right_3, %rsi # Source
	rep movsq
	addq $scan_subtraction_minuends_threes, 17(%r13) # Insert minuend address
	addq $INSTRUCTION_SIZE_SCAN_RIGHT_3, %r13
.endm

.equ INSTRUCTION_SIZE_SCAN_LEFT_3, 123
.align 8
byte_code_scan_left_3:
	.byte 0x48, 0xC7, 0xC1, 0x04, 0x00, 0x00, 0x00			#0   movq $4, %rcx
	.byte 0x80, 0xE9, 0x02									#7   1: subb $2, %cl
	.byte 0x79, 0x03										#10  jns 2f
	.byte 0x80, 0xC1, 0x03									#12  addb $3, %cl

	.byte 0xC5, 0xFE, 0x6F, 0x81, 0x00, 0x00, 0x00, 0x00	#15  2: vmovdqu scan_subtraction_minuends_threes(%rcx), %ymm0
	.byte 0xC4, 0xC1, 0x7E, 0x6F, 0x4C, 0x24, 0xE1			#23  vmovdqu -31(%r12), %ymm1
	.byte 0xC5, 0xFD, 0xD8, 0xE1							#30  vpsubusb %ymm1, %ymm0, %ymm4
	.byte 0xC4, 0xE3, 0x7D, 0x39, 0xE2, 0x01				#34  vextracti128 $1, %ymm4, %xmm2
	.byte 0xC5, 0xF1, 0x73, 0xDA, 0x08, 0x66				#40  vpsrldq $8, %xmm2, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8							#46  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0									#50  testq %r8, %r8
	.byte 0x75, 0x35										#53  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08, 0x66						#55  subq $8, %r12
	.byte 0x49, 0x0F, 0x7E, 0xD0							#60  movq %xmm2, %r8
	.byte 0x4D, 0x85, 0xC0									#64  testq %r8, %r8
	.byte 0x75, 0x27										#67  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08							#69  subq $8, %r12
	.byte 0xC5, 0xE1, 0x73, 0xDC, 0x08, 0x66				#73  vpsrldq $8, %xmm4, %xmm3
	.byte 0x49, 0x0F, 0x7E, 0xD8							#79  movq %xmm3, %r8
	.byte 0x4D, 0x85, 0xC0									#83  testq %r8, %r8
	.byte 0x75, 0x14										#86  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08, 0x66						#88  subq $8, %r12
	.byte 0x49, 0x0F, 0x7E, 0xE0							#93  movq %xmm4, %r8
	.byte 0x4D, 0x85, 0xC0									#97  testq %r8, %r8
	.byte 0x75, 0x06										#100 jnz 1f
	.byte 0x49, 0x83, 0xEC, 0x08							#102 subq $8, %r12
	.byte 0xEB, 0x9B										#106 jmp 1b

	.byte 0x4D, 0x0F, 0xBD, 0xC0							#108 1: bsrq %r8, %r8
	.byte 0x49, 0xC1, 0xE8, 0x03							#112 shrq $3, %r8
	.byte 0x4D, 0x01, 0xC4									#116 addq %r8, %r12
	.byte 0x49, 0x83, 0xEC, 0x07							#119 subq $7, %r12
	.byte 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00			# Padding
.macro write_instruction_scan_left_3
	movq $16, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_scan_left_3, %rsi # Source
	rep movsq
	addq $scan_subtraction_minuends_threes, 19(%r13) # Insert minuend address
	addq $INSTRUCTION_SIZE_SCAN_LEFT_3, %r13
.endm

.equ INSTRUCTION_SIZE_SCAN_RIGHT_POW2, 107
.align 8
byte_code_scan_right_pow2:
	.byte 0x49, 0xC7, 0xC0, 0x00, 0x00, 0x00, 0x00			#0   movq $minuend_index, %r8
	.byte 0xC4, 0xC1, 0x7D, 0x6F, 0x00						#7   vmovdqa (%r8), %ymm0

	.byte 0xC4, 0xC1, 0x7E, 0x6F, 0x0C, 0x24				#12  1: vmovdqu (%r12), %ymm1
	.byte 0xC5, 0xFD, 0xD8, 0xC9, 0x66						#18  vpsubusb %ymm1, %ymm0, %ymm1
	.byte 0x49, 0x0F, 0x7E, 0xC8							#23  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0									#27  testq %r8, %r8
	.byte 0x75, 0x40										#30  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08							#32  addq $8, %r12
	.byte 0xC5, 0xE9, 0x73, 0xD9, 0x08, 0x66				#36  vpsrldq $8, %xmm1, %xmm2
	.byte 0x49, 0x0F, 0x7E, 0xD0							#42  movq %xmm2, %r8
	.byte 0x4D, 0x85, 0xC0									#46  testq %r8, %r8
	.byte 0x75, 0x2D										#49  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08							#51  addq $8, %r12
	.byte 0xC4, 0xE3, 0x7D, 0x39, 0xC9, 0x01, 0x66			#55  vextracti128 $1, %ymm1, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8							#62  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0									#66  testq %r8, %r8
	.byte 0x75, 0x19										#69  jnz 1f

	.byte 0x49, 0x83, 0xC4, 0x08							#71  addq $8, %r12
	.byte 0xC5, 0xF1, 0x73, 0xD9, 0x08, 0x66				#75  vpsrldq $8, %xmm1, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8							#81  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0									#85  testq %r8, %r8
	.byte 0x75, 0x06										#88  jnz 1f
	.byte 0x49, 0x83, 0xC4, 0x08							#90  addq $8, %r12
	.byte 0xEB, 0xAC										#94  jmp 1b

	.byte 0x4D, 0x0F, 0xBC, 0xC0							#96  1: bsrq %r8, %r8
	.byte 0x49, 0xC1, 0xE8, 0x03							#100 shrq $3, %r8
	.byte 0x4D, 0x01, 0xC4									#104 addq %r8, %r12
	.byte 0x00, 0x00, 0x00, 0x00, 0x00						# Padding
.macro write_instruction_scan_right_pow2 right_count
	movl \right_count, %r8d # Get minuend index
	shlq $5, %r8
	addq $scan_subtraction_minuends_right, %r8
	movq $14, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_scan_right_pow2, %rsi # Source
	rep movsq
	movl %r8d, 3(%r13) # Insert minuend index
	addq $INSTRUCTION_SIZE_SCAN_RIGHT_POW2, %r13
.endm

.equ INSTRUCTION_SIZE_SCAN_LEFT_POW2, 112
.align 8
byte_code_scan_left_pow2:
	.byte 0x49, 0xC7, 0xC0, 0x00, 0x00, 0x00, 0x00			#0   movq $minuend_index, %r8
	.byte 0xC4, 0xC1, 0x7D, 0x6F, 0x00						#7   vmovdqa (%r8), %ymm0

	.byte 0xC4, 0xC1, 0x7E, 0x6F, 0x4C, 0x24, 0xE1			#12  1: vmovdqu -31(%r12), %ymm1
	.byte 0xC5, 0xFD, 0xD8, 0xE1							#19  vpsubusb %ymm1, %ymm0, %ymm4
	.byte 0xC4, 0xE3, 0x7D, 0x39, 0xE2, 0x01				#23  vextracti128 $1, %ymm4, %xmm2
	.byte 0xC5, 0xF1, 0x73, 0xDA, 0x08, 0x66				#29  vpsrldq $8, %xmm2, %xmm1
	.byte 0x49, 0x0F, 0x7E, 0xC8							#35  movq %xmm1, %r8
	.byte 0x4D, 0x85, 0xC0									#39  testq %r8, %r8
	.byte 0x75, 0x35										#42  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08, 0x66						#44  subq $8, %r12
	.byte 0x49, 0x0F, 0x7E, 0xD0							#49  movq %xmm2, %r8
	.byte 0x4D, 0x85, 0xC0									#53  testq %r8, %r8
	.byte 0x75, 0x27										#56  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08							#58  subq $8, %r12
	.byte 0xC5, 0xE1, 0x73, 0xDC, 0x08, 0x66				#62  vpsrldq $8, %xmm4, %xmm3
	.byte 0x49, 0x0F, 0x7E, 0xD8							#68  movq %xmm3, %r8
	.byte 0x4D, 0x85, 0xC0									#72  testq %r8, %r8
	.byte 0x75, 0x14										#75  jnz 1f

	.byte 0x49, 0x83, 0xEC, 0x08, 0x66						#77  subq $8, %r12
	.byte 0x49, 0x0F, 0x7E, 0xE0							#82  movq %xmm4, %r8
	.byte 0x4D, 0x85, 0xC0									#86  testq %r8, %r8
	.byte 0x75, 0x06										#89  jnz 1f
	.byte 0x49, 0x83, 0xEC, 0x08							#91  subq $8, %r12
	.byte 0xEB, 0xAB										#95  jmp 1b

	.byte 0x4D, 0x0F, 0xBD, 0xC0							#97  1: bsrq %r8, %r8
	.byte 0x49, 0xC1, 0xE8, 0x03							#101 shrq $3, %r8
	.byte 0x4D, 0x01, 0xC4									#105 addq %r8, %r12
	.byte 0x49, 0x83, 0xEC, 0x07							#108 subq $7, %r12
.macro write_instruction_scan_left_pow2 left_count
	movl \left_count, %r8d # Get minuend index
	shlq $5, %r8
	addq $scan_subtraction_minuends_left, %r8
	movq $14, %rcx # Quad count
	movq %r13, %rdi # Destination
	movq $byte_code_scan_left_pow2, %rsi # Source
	rep movsq
	movl %r8d, 3(%r13) # Insert minuend index
	addq $INSTRUCTION_SIZE_SCAN_LEFT_POW2, %r13
.endm

.macro write_scan right_count
	# Get absolute value
	movl \right_count, %r8d
	cmpl $0, %r8d
	jge 1f
	negl %r8d
1:

	# Case greater than 4
	cmpl $4, %r8d
	jbe 1f
	write_instruction_scan_loop \right_count
	jmp 3f
1:

	# Case 3
	cmpl $3, %r8d
	jne 1f
	cmpl $0, \right_count # Check if right count is negative
	jge 2f
	write_instruction_scan_left_3
	jmp 3f
2:
	write_instruction_scan_right_3
	jmp 3f
1:

	# Case power of 2
	cmpl $0, \right_count # Check if right count is negative
	jge 1f
	write_instruction_scan_left_pow2 %r8d
	jmp 3f
1:
	write_instruction_scan_right_pow2 %r8d
	jmp 3f
3:
.endm


##############################################################################################################################################
# Second pass
##############################################################################################################################################
compile_second_pass:
	# %r12 becomes the intermediate source
	# %r13 becomes the executable source block
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

	# Jump into jump table
	movzb %dl, %rax
	shlq $3, %rax
	jmp *sp_compile_jmp_table(%rax)


/*
	Each instruction contains 8 bytes: the first is op code. The next is the amount, regardless of if the instruction requires it.
	The next 4 bytes are the memory pointer offset, regardless of if the instruction requires it. And the final 2 bytes contain
	some flags the instruction might use.
*/
.equ OP_CODE_EXIT, 0
.equ OP_CODE_PLUS, 1
.equ OP_CODE_RIGHT, 2
.equ OP_CODE_IN, 3
.equ OP_CODE_OUT, 4
.equ OP_CODE_IF, 5
.equ OP_CODE_FOR, 6
.equ OP_CODE_SET, 7
.equ OP_CODE_LOAD_LOOP_COUNT, 8
.equ OP_CODE_MULT_ADD, 9
.equ OP_CODE_SCAN, 10


sp_compile_jmp_table:
	.quad sp_compile_exit
	.quad sp_compile_plus
	.quad sp_compile_right
	.quad sp_compile_in
	.quad sp_compile_out
	.quad sp_compile_if
	.quad sp_compile_for
	.quad sp_compile_set
	.quad sp_compile_load_loop_count
	.quad sp_compile_mult_add
	.quad sp_compile_scan


sp_compile_exit:
	# Write ret
	movb $0xC3, (%r13)

	SET_INTERMEDIATE_SRC_SIZE_STAT # Comment out above
	DECOMPILER # Comment out above
	GET_TIME # Comment out above

	jmp execute

sp_compile_plus:
	movb %dh, %al # Get amount
	shrq $16, %rdx # Get memory pointer offset
	write_instruction_plus %al, %edx
	jmp compile_second_pass_loop

sp_compile_right:
	shrq $16, %rdx # Get amount
	write_instruction_right %edx
	jmp compile_second_pass_loop

sp_compile_in:
	shrq $16, %rdx # Get amount
	write_instruction_in %edx
	jmp compile_second_pass_loop

sp_compile_out:
	shrq $16, %rdx # Get memory pointer offset
	write_instruction_out %edx
	jmp compile_second_pass_loop

sp_compile_if:
	write_instruction_if
	pushq %r13 # Push address of first instruction after if instruction
	jmp compile_second_pass_loop

sp_compile_for:
	write_instruction_for
	popq %rdx # Get address of first instruction after if instruction
	write_jump_offset %r13, -4, %rdx
	write_jump_offset %rdx, -4, %r13
	jmp compile_second_pass_loop
	
sp_compile_set:
	movb %dh, %al # Get amount
	shrq $16, %rdx # Get memory pointer offset
	write_instruction_set %al, %edx
	jmp compile_second_pass_loop

sp_compile_load_loop_count:
	movb %dh, %al # Get amount
	shrq $16, %rdx # Get memory pointer offset
	write_load_loop_count %al, %edx
	jmp compile_second_pass_loop

sp_compile_mult_add:
	movb %dh, %al # Get amount
	shrq $16, %rdx # Get memory pointer offset
	write_mult_add %al, %edx
	jmp compile_second_pass_loop

sp_compile_scan:
	shrq $16, %rdx # Get memory pointer offset
	write_scan %edx
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
