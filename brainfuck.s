.global brainfuck
.global putchar # Tell the linker to link these two symbols so that the
.global getchar # executable memory can use them
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
runtime_memory: .skip 30000, 0

.text



##############################################################################################################################################
# Brainfuck
##############################################################################################################################################
.macro compile_char_simple char jmp_table_index initial_repetition_count
compile_char_\char:
	movq %r15, %rax # Copy last read instruction
	movq %rbx, %rdx # Copy instruction repetition counter into %rdx for the intructions to use
	movq $\jmp_table_index, %r15 # Set last read instruction
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
	push %r12 # -8  Becomes intermediate_src counter
	push %r13 # -24 Becomes pointer to executable source block
	push %r14 # -16 Becomes brainfuck source pointer
	push %r15 # -32 Becomes last read instruction
	push %rbx # -40 Becomes instruction repetition counter

	movq %rdi, %r14 # Save brainfuck source pointer

	# Allocate executable memory
    movq $9, %rax      # Syscall number for mmap
    movq $0, %rdi      # Addr = NULL
    movq $0x100000, %rsi # Set length to 65546
    movq $0x7, %rdx    # Prot = PROT_READ | PROT_WRITE | PROT_EXEC
    movq $0x62, %r10   # Flags = MAP_ANONYMOUS | MAP_PRIVATE | MAP_32BIT
    movq $-1, %r8      # Fd = -1 (no file descriptor)
    movq $0, %r9       # Offset = 0
    syscall
	movq %rax, %r13 # Save executable source block

	# Each bracket frame contains 24 bytes. The first quad contain the previous %rbp. The next 4 are used to the address of the if statement. 
	# The next 4 bytes are used to keep track of the total memory pointer movement. The last 4 bytes are used to keep track of some loop
	# optimization possibilities.
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

	# Init %r12-15 and rbx
	movq $0, %r12
	movq $read_loop, %r15 # Last read instruction
	movq $0, %rbx # The instruction repetition counter
	decq %r14 # Decrement brainfuck source by one to increment to 0 in loop
read_loop:
	# Get next char
	incq %r14
	movb (%r14), %al

	# Check all the chars that can appear in the program
	cmpb $0, %al	# Check for null termination character
	je compile_char_null
	cmpb $0x5b, %al	# Check for [
	je compile_char_if
	cmpb $0x5d, %al	# Check for ]
	je compile_char_for
	cmpb $0x3c, %al	# Check for <
	je compile_char_left_repetition
	cmpb $0x3e, %al	# Check for >
	je compile_char_right_repetition
	cmpb $0x2d, %al	# Check for -
	je compile_char_minus_repetition
	cmpb $0x2b, %al	# Check for +
	je compile_char_plus_repetition
	cmpb $0x2c, %al	# Check for ,
	je compile_char_in
	cmpb $0x2e, %al	# Check for .
	je compile_char_out
	jmp read_loop # If it is none of those chars, it is either a comment or whitespace, so just ignore it

	compile_char_simple if compile_if 1
	compile_char_simple for compile_for 1
	compile_char_simple left compile_right -1
	compile_char_simple right compile_right 1
	compile_char_simple minus compile_plus -1
	compile_char_simple plus compile_plus 1
	compile_char_simple in compile_in 1
	compile_char_simple out compile_out 1

compile_char_null:
	decq %r14 # Decrement src ptr to guarantee that the next loop cicle will also read a null termination character
	compile_char_simple return compile_return 1

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

compile_return:
	# Write ret instruction
	movb $0xC3, (%r13, %r12)

	# Pop final bracket frame
	movq %rbp, %rsp
	popq %rbp

	SET_INTERMEDIATE_SRC_SIZE_STAT # Comment out above
	DECOMPILER # Comment out above
	GET_TIME # Comment out above

	# Call the executable source block
	movq $runtime_memory, %r12
	call *%r13

	# Restore %r12-15 and %rbx
	movq -8(%rbp), %r12
	movq -16(%rbp), %r13
	movq -24(%rbp), %r14
	movq -32(%rbp), %r15
	movq -40(%rbp), %rbx
	EPILOGUE


##############################################################################################################################################
# Write instruction
##############################################################################################################################################
.equ INSTRUCTION_SIZE_RIGHT, 7
/*
	#0 41 81 C4 .skip 4 (amount)		addl $amount, %r12
*/
.macro write_instruction_right amount
	movl $0x00C48141, (%r13, %r12)
	movl \amount, 3(%r13, %r12)
	addq $INSTRUCTION_SIZE_RIGHT, %r12
.endm

.equ INSTRUCTION_SIZE_PLUS, 9
/*
	#0 41 80 84 24 .skip 4 (address) .skip 1 (amount)		addq amount, address(%r12)
*/
.macro write_instruction_plus amount, address
	movq $0x24848041, (%r13, %r12)
	movl \address, 4(%r13, %r12)
	movb \amount, 8(%r13, %r12)
	addq $INSTRUCTION_SIZE_PLUS, %r12
.endm

.equ INSTRUCTION_SIZE_IN, 13
/*
	#0 E8 .skip 4 (address offset)			call getchar
	#5 41 88 84 24 .skip 4 (address)		movb %al, address(%r12)
*/
.macro write_instruction_in address
	movb $0xE8, (%r13, %r12)
	movl $0x24848841, 5(%r13, %r12)
	movl \address, 9(%r13, %r12)

	# Calculate address offset
	movl $getchar - 5, %eax # Get target address minus index of first byte after call
	subl %r12d, %eax # Get offset from start of this instruction write
	subl %r13d, %eax # Get offset from start of executable memory
	movl %eax, 1(%r13, %r12) # Insert address offset into instruction
	addq $INSTRUCTION_SIZE_IN, %r12
.endm

.equ INSTRUCTION_SIZE_OUT, 13
/*
	#0 41 8A BC 24 .skip 4 (address)		movb address(%r12), %dil
	#8 E8 .skip 4 (address offset)			call putchar
*/
.macro write_instruction_out address
	movl $0x24BC8A41, (%r13, %r12)
	movl \address, 4(%r13, %r12)
	movb $0xE8, 8(%r13, %r12)

	# Calculate address offset
	movl $putchar - 13, %eax # Get target address minus index of first byte after call
	subl %r12d, %eax # Get offset from start of this instruction write
	subl %r13d, %eax # Get offset from start of executable memory
	movl %eax, 9(%r13, %r12) # Insert address offset into instruction
	addq $INSTRUCTION_SIZE_OUT, %r12
.endm

.equ INSTRUCTION_SIZE_IF, 11
/*
	#0 41 80 3C 24 00					cmpb $0, (%r12)
	#5 0F 84 .skip 4 (address offset)	je(long jump) address
*/
.macro write_instruction_if
	movl $0x243C8041, (%r13, %r12)
	movl $0x00840F00, 4(%r13, %r12)
	addq $INSTRUCTION_SIZE_IF, %r12
.endm

.equ INSTRUCTION_SIZE_FOR, 11
/*
	#0 41 80 3C 24 00					cmpb $0, (%r12)
	#5 0F 85 .skip 4 (address offset)	jne(long jump) address
*/
.macro write_instruction_for
	movl $0x243C8041, (%r13, %r12)
	movl $0x00850F00, 4(%r13, %r12)
	addq $INSTRUCTION_SIZE_FOR, %r12
.endm



# Writes the jump offset at source to be the offset to dest
.macro write_jump_offset from, static_offset_write, dest
	# Calculate address offset
	movl \dest, %eax # Get target
	subl \from, %eax # Subtract source
	movl %eax, \static_offset_write(%r13d, \from)
.endm



# Multiplication first loads the amount the addition is repeated into a dedicated register, 
# then adds the mult factor * this repetition amount to the memory at the memory pointer offset.


##############################################################################################################################################
# Compile
##############################################################################################################################################
.macro compile_pop_mem_movement
	movl -4(%rbp), %eax # Get memory pointer offset from bracket frame
	cmpl $0, %eax # If memory pointer offset is 0, skip
	je 1f

	movl $0, -4(%rbp) # Make memory pointer offset 0
	write_instruction_right %eax # Write instruction
1:
.endm

.macro remove_move_instruction_via_r8 src_pointer
	// # Check if there is a move instruction the if to remove
	// movq intermediate_src - INSTRUCTION_SIZE_RIGHT(\src_pointer), %r8 # Get instruction before if
	// cmpb $OP_CODE_RIGHT, %r8b # Check if instruction is move instruction
	// jne 1f

	// # Remove move instruction
	// shrq $8, %r8 # Get move amount
	// addl %r8d, 12(%rsp) # Increment memory pointer offset by move amount
	// subl $INSTRUCTION_SIZE_RIGHT, \src_pointer # Move given src pointer back
1:
.endm

.equ LOOP_CONTAINS_RIGHT, 0x1
.equ LOOP_CONTAINS_PLUS, 0x2
.equ LOOP_CONTAINS_IN, 0x4
.equ LOOP_CONTAINS_OUT, 0x8
.equ LOOP_CONTAINS_SET_ZERO, 0x10
.equ LOOP_CONTAINS_LOOP, 0x20
compile_if:
	compile_pop_mem_movement

	pushq %rbp
	movq %rsp, %rbp
	pushq %r12 # Push new empty bracket frame
	pushq $0
	// write_instruction_if
	movl $0x243C8041, (%r13, %r12)
	view:
	movl $0x00840F00, 4(%r13, %r12)
	addq $INSTRUCTION_SIZE_IF, %r12

	jmp read_loop

compile_for:
// 	movb (%rsp), %al # Get loop optimization flags from bracket frame
// 	movb %al, %ah # Both al and ah contain the flags, al is used to check, ah for quick access
// 	andb $LOOP_OPPTIMIZATION_SET_ZERO, %al # Isolate set zero flag
// 	cmpb $0, %al # If set zero flag is not set, skip optimize loop
// 	je check_loop_optimization_mult

// 	# If we get here, that means that the loop contains only one plus instruction. So we skip 
// 	# INSTRUCTION_SIZE_IF + INSTRUCTION_SIZE_PLUS bytes back and insert a set zero instruction.
// 	addq $16, %rsp # Pop bracket frame
// 	subq $INSTRUCTION_SIZE_IF + INSTRUCTION_SIZE_PLUS, %r12 # Move intermediate src pointer back
// 	remove_move_instruction_via_r8 %r12d # Remove move instruction using %r12d as the src pointer

// 	movl 12(%rsp), %eax # Write offset from memory pointer
// 	write_instruction_long SET_ZERO %eax
// 	andb $0, (%rsp) # No loop optimizations possible
// 	jmp read_loop

// check_loop_optimization_mult:
// 	movb %ah, %al # Copy flags to al
// 	andb $LOOP_OPPTIMIZATION_MULT, %al # Isolate mult flag
// 	cmpb $0, %al # If mult flag is not set, skip optimize loop
// 	je check_loop_optimization_scan_memory

// 	# Check if the total memory movement is 0, if so, skip optimize loop.
// 	cmpl $0, 12(%rsp)
// 	jne check_loop_optimization_scan_memory

// 	/*
// 		If we got here, that means the loop contains only plus instructions and the net total memory movement is not 0.
// 		In other words, this loop executes a multiplication(s). First we check if we need to remove a move before the if. 
// 		Then we move to the start of the loop, next we check the memory pointer offset of each plus instruction. 
// 		If the offset is 0, we found a source add instruction and keep track of it. If the offset is not 0, 
// 		we found a destination and overwrite the instruction we just read with a mult instruction. %rax will contain the 
// 		location of the if instruction, and keep it. %rcx will contain the instruction after the if and will increment 
// 		to read the plus instructins. %12 will start of INSTRUCTION_SIZE_LOAD_LOOP_COUNT away from the if statement to keep room 
// 		for the load loop count instruction, and then write the mult instructions. %rdx will contain the location of the for instruction 
// 		to keep track of the end of the loop. Finally, %r11 will store the memory pointer offset originating form the parent 
// 		bracket frame and %r8 will keep track of the source add count.
// 	*/

// 	# Assignments before the move check
// 	movl 8(%rsp), %eax # Get address of if instruction, %rax will keep it
// 	movl %eax, %ecx # Copy %rax to %rcx, %rcx will increment to read the plus instructins
// 	addl $INSTRUCTION_SIZE_IF, %ecx # Move %rcx by INSTRUCTION_SIZE_IF
// 	addq $16, %rsp # Pop bracket frame

// 	# Remove move instruction using %eax as the src pointer
// 	remove_move_instruction_via_r8 %eax

// 	# Assignments after the move check
// 	movl %r12d, %edx # Copy %r12 to %rdx, %rdx will keep track of the end of the loop
// 	movl %eax, %r12d # Move intermediate src pointer to instruction after if
// 	addl $INSTRUCTION_SIZE_LOAD_LOOP_COUNT, %r12d # Move %r12 by INSTRUCTION_SIZE_LOAD_LOOP_COUNT
// 	movl 12(%rsp), %r11d # Write offset from memory pointer
// 	movl $0, %r8d # Set source add count to 0
// optimize_mult_loop_loop:
// 	# Read plus instruction
// 	movq 1 + intermediate_src(%ecx), %r9 # Read plus instruction payload into %r9, %r9 will contain the amount
// 	movq %r9, %r10 # Copy %r9 to %r10, %r10 will contain the memory pointer offset
// 	shr $8, %r10 # Get memory pointer offset

// 	# Check if the memory pointer offset of plus instruction is 0
// 	cmpl $0, %r10d # If memory pointer offset is 0, it is a source add instruction
// 	jne optimize_mult_loop_not_source_add

// 	# Read the source add count
// 	addb %r9b, %r8b # Add source add count to source add count
// 	jmp optimize_mult_loop_loop_end_condition

// optimize_mult_loop_not_source_add:
// 	# Write mult instruction
// 	addl %r11d, %r10d # Add memory pointer offset of bracket frame to this memory pointer offset
// 	write_instruction_byte_long MULT_ADD %r9b %r10d
// 	jmp optimize_mult_loop_loop_end_condition

// optimize_mult_loop_loop_end_condition:
// 	# End condition
// 	addl $INSTRUCTION_SIZE_PLUS, %ecx # Increment %rcx by INSTRUCTION_SIZE_PLUS
// 	cmpl %ecx, %edx # If we haven't reached the if instruction, keep looping
// 	jne optimize_mult_loop_loop

// 	# Write load loop count instruction
// 	movb $OP_CODE_LOAD_LOOP_COUNT, intermediate_src(%eax) # Write op code to instruction
// 	movb %r8b, 1 + intermediate_src(%eax) # Write source add count to instruction
// 	movl %r11d, 2 + intermediate_src(%eax)

// 	# Write set zero instruction
// 	write_instruction_long SET_ZERO %r11d
// 	andb $0, (%rsp) # No loop optimizations possible
// 	jmp read_loop


// check_loop_optimization_scan_memory:
// 	movb %ah, %al # Copy flags to al
// 	andb $LOOP_OPPTIMIZATION_SCAN_MEMORY, %al # Isolate scan memory flag
// 	cmpb $0, %al # If scan memory flag is not set, skip optimize loop
// 	je compile_for_no_optimizations
	
// 	movl 12(%rsp), %eax # Read memory pointer offset of left instruction
// 	subl $INSTRUCTION_SIZE_IF, %r12d # Go back to the if instruction
// 	addq $16, %rsp # Pop bracket frame
// 	andb $0, (%rsp) # No loop optimizations possible

// 	# If we got here, that means the loop contains a singular memory movement instruction. If the memory offset is withing the 
// 	# range of -4 to 4, inclusive, then we can use vector registers. If the values are 1, 2, 4, -1, -2 or -4, they divide 64 evenly,
// 	# so we can use an optimized instruction to take advantage of that. Then the minduend indices should be 0, 32, 96, 64, 128, 160 
// 	# consecutively. Otherwise we have to think about the remainders of the mod3 arithmatic. Finally, if it doesn't fit in the range,
// 	# we'll have to do a more manual scan.

// 	cmpl $0, %eax # If greater than zero, do checks for positive numbers, else for negative
// 	jl check_loop_optimization_scan_negatives
// 	cmpl $4, %eax # Check if it is a manual scan
// 	jg optimize_loop_scan_manual
// 	cmpl $3, %eax # Check if it is a three scan loop
// 	je optimize_loop_scan_positive_three

// 	# It is a simplified right scan instruction
// 	shlb $5, %al # Multiply the memory pointer offset by 32 to get the minuend index (32, 64, 128)
// 	movb %al, 1 + intermediate_src(%r12) # Insert minuend index into instruction
// 	write_instruction SCAN_RIGHT_POW2
// 	jmp read_loop

// check_loop_optimization_scan_negatives:
// 	cmpl $-4, %eax # Check if it is a manual scan
// 	jl optimize_loop_scan_manual
// 	cmpl $-3, %eax # Check if it is a three scan loop
// 	je optimize_loop_scan_negative_three

// 	# Write a pow 2 scan loop
// 	negl %eax # Negate memory pointer offset to make it positive
// 	incb %al # Increment it to make it 2, 3 or 5
// 	cmpb $2, %al # Check if the movement amount is 2
// 	jne 1f
// 	movb $0, %al # If so, set it to 0
// 1:
// 	shlb $5, %al # Multiply by 32 to get the minuend index (0, 96, 160)
// 	movb %al, 1 + intermediate_src(%r12) # Insert minuend index into instruction
// 	write_instruction SCAN_LEFT_POW2
// 	jmp read_loop

// optimize_loop_scan_positive_three:
// 	write_instruction SCAN_RIGHT_THREE
// 	jmp read_loop

// optimize_loop_scan_negative_three:
// 	write_instruction SCAN_LEFT_THREE
// 	jmp read_loop

// optimize_loop_scan_manual:
// 	movl %eax, 1 + intermediate_src(%r12) # Insert memory pointer offset into instruction
// 	write_instruction SCAN_MANUAL
// 	jmp read_loop

compile_for_no_optimizations:
	compile_pop_mem_movement

	write_instruction_for

	movl -8(%rbp), %edx # Get address of if instruction
	addl $INSTRUCTION_SIZE_IF, %edx # Move address to instruction after if
	write_jump_offset %r12d, -4, %edx
	write_jump_offset %edx, -4, %r12d

	movq %rbp, %rsp # Pop bracket frame
	popq %rbp

	orl $LOOP_CONTAINS_LOOP, -12(%rbp) # Loop contains a inner loop
	jmp read_loop

compile_right:
	addl %edx, -4(%rbp) # Increment memory pointer offset in bracket frame
	orl $LOOP_CONTAINS_RIGHT, -12(%rbp) # Loop contains a right instruction
	jmp read_loop

compile_plus:
	movl -4(%rbp), %eax # Get offset from memory pointer
	write_instruction_plus %dl %eax
	orl $LOOP_CONTAINS_PLUS, -12(%rbp) # Loop contains a plus instruction
	jmp read_loop

compile_in:
	movl -4(%rbp), %eax # Get offset from memory pointer
	write_instruction_in %eax
	orl $LOOP_CONTAINS_IN, -12(%rbp) # Loop contains a in instruction
	jmp read_loop

compile_out:
	movl -4(%rbp), %eax # Get offset from memory pointer
	write_instruction_out %eax
	orl $LOOP_CONTAINS_OUT, -12(%rbp) # Loop contains a out instruction
	jmp read_loop


##############################################################################################################################################
# Run
##############################################################################################################################################
// run:
// 	PROLOGUE

// 	# Free up %r12-15
// 	push %r12 # -8  Becomes intermediate_src counter
// 	push %r13 # -16 Becomes memory pointer
// 	push %r14 # -24 Becomes output counter
// 	push %r15 # -32 Becomes variable register for some instructions
// 	# %rcx is used as instruction register

// 	# Init intermediate_src counter, memory pointer and output counter to 0
// 	movq $0, %r12
// 	movq $0, %r13
// 	movq $0, %r14
// run_loop:
// 	INCR_EXECUTED_OPERATIONS_STAT # Comment out above

// 	# Jump into instruction table
// 	movq intermediate_src(%r12), %rcx # Get instruction
// 	movzb %cl, %rdx # Read op code
// 	shlq $3, %rdx # Multiply by 8
// 	jmp *run_instruction_jmp_table(%rdx) # Index into the table

// run_return:
// 	# Restore %r12-15
// 	movq -8(%rbp), %r12	
// 	movq -16(%rbp), %r13
// 	movq -24(%rbp), %r14
// 	movq -32(%rbp), %r15
// 	EPILOGUE


// .equ OP_CODE_EXIT, 0
// .equ OP_CODE_IF, 1
// .equ OP_CODE_FOR, 2
// .equ OP_CODE_RIGHT, 3
// .equ OP_CODE_SCAN_RIGHT_POW2, 4
// .equ OP_CODE_SCAN_LEFT_POW2, 5
// .equ OP_CODE_SCAN_RIGHT_THREE, 6
// .equ OP_CODE_SCAN_LEFT_THREE, 7
// .equ OP_CODE_SCAN_MANUAL, 8
// .equ OP_CODE_LOAD_LOOP_COUNT, 26
// .equ OP_CODE_PLUS, 27
// .equ OP_CODE_SET_ZERO, 28
// .equ OP_CODE_MULT_ADD, 29
// .equ OP_CODE_IN, 30
// .equ OP_CODE_OUT, 31
// run_instruction_jmp_table:
// 	.quad run_return				# 0 exit
// 	.quad run_instruction_if		# 1 if
// 	.quad run_instruction_for		# 2 for
// 	.quad run_instruction_right		# 3 right
// 	.quad run_instruction_scan_right_pow2	# 4
// 	.quad run_instruction_scan_left_pow2	# 5
// 	.quad run_instruction_scan_right_three	# 6
// 	.quad run_instruction_scan_left_three	# 7
// 	.quad run_instruction_scan_manual		# 8
// 	.quad run_return				# 9
// 	.quad run_return				# 10
// 	.quad run_return				# 11
// 	.quad run_return				# 12
// 	.quad run_return				# 13
// 	.quad run_return				# 14
// 	.quad run_return				# 15
// 	.quad run_return				# 16
// 	.quad run_return				# 17
// 	.quad run_return				# 18
// 	.quad run_return				# 19
// 	.quad run_return				# 20
// 	.quad run_return				# 21
// 	.quad run_return				# 22
// 	.quad run_return				# 23
// 	.quad run_return				# 24
// 	.quad run_return				# 25
// 	.quad run_instruction_load_loop_count # 26 load loop count
// 	.quad run_instruction_plus		# 27 plus
// 	.quad run_instruction_set_zero	# 28 set zero
// 	.quad run_instruction_mult_add	# 29 mult add
// 	.quad run_instruction_in		# 30 in
// 	.quad run_instruction_out		# 31 out



// /*
// 	Scans the ymm1 register for the first byte whose value is not 0. The scan can go both ways by substituting either addq or subq into the
// 	first parameter, and bsfq or bsrq into the second parameter, and changing the 4th, 5th, 6th and 7th parameters to the appropriate
// 	instructions to extract the right 64 bits from the ymm1 register. The macro assumes that these results end up in xmm1, xmm2, xmm3 and xmm4
// 	consecutively. Note though that in reverse the result in the memory pointer will be one to big. The third parameter is the label to jump 
// 	to if no match is found, potentially so this macro can be used in a loop. Even if no match is found, the memory pointer will still 
// 	be incremented/decremented by 32.
// */
// .macro scan_ymm1_for_0s add_or_sub_instruction bsr_or_f_instruction no_ones_label instr_extract_0 instr_extract_1 instr_extract_2 instr_extract_3
// 	\instr_extract_0 # Extract first 8 byte
// 	movq %xmm1, %rax # Get lower half
// 	cmpq $0, %rax # Check if any byte is 1
// 	jne 1f # Check the 64 msb

// 	\add_or_sub_instruction $8, %r13d # Increment/decrement memory pointer by 8
// 	\instr_extract_1 # Extract next 8 byte
// 	movq %xmm2, %rax # Get lower half
// 	cmpq $0, %rax # Check if any byte is 1
// 	jne 1f # Check the 64 msb

// 	\add_or_sub_instruction $8, %r13d # Increment/decrement memory pointer by 8
// 	\instr_extract_2 # Extract next 8 byte
// 	movq %xmm3, %rax # Get lower half
// 	cmpq $0, %rax # Check if any byte is 1
// 	jne 1f # Check the 64 msb

// 	\add_or_sub_instruction $8, %r13d # Increment/decrement memory pointer by 8
// 	\instr_extract_3 # Extract last 8 byte
// 	movq %xmm4, %rax # Get lower half
// 	cmpq $0, %rax # Check if any byte is 1
// 	jne 1f # Check the 64 msb
// 	\add_or_sub_instruction $8, %r13d # Increment/decrement memory pointer by 8
// 	jmp \no_ones_label # No ones

// 1:
// 	\bsr_or_f_instruction %rax, %rax # Get index of the first non zero bit
// 	shrq $3, %rax # Divide by 8
// 	addl %eax, %r13d # Add to %r15
// .endm

// .align 32
// scan_subtraction_minuends:
// 	.quad 0x0101010101010101, 0x0101010101010101, 0x0101010101010101, 0x0101010101010101 # 0   Left 1
// 	.quad 0x0101010101010101, 0x0101010101010101, 0x0101010101010101, 0x0101010101010101 # 32  Right 1
// 	.quad 0x0001000100010001, 0x0001000100010001, 0x0001000100010001, 0x0001000100010001 # 64  Right 2
// 	.quad 0x0100010001000100, 0x0100010001000100, 0x0100010001000100, 0x0100010001000100 # 96  Left 2
// 	.quad 0x0000000100000001, 0x0000000100000001, 0x0000000100000001, 0x0000000100000001 # 128 Right 4
// 	.quad 0x0100000001000000, 0x0100000001000000, 0x0100000001000000, 0x0100000001000000 # 160 Left 4
// 	.quad 0x0001000001000001, 0x0100000100000100, 0x0000010000010000, 0x0001000001000001 # 192 Three's
// 	.word 0x0100
	
// run_instruction_if:
// 	addq $INSTRUCTION_SIZE_IF, %r12 # Increment intermediate src pointer regardless
// 	cmpb $0, runtime_memory(%r13) # Check if it needs to jump
// 	jne run_loop # No jump, just continue with next instruction

// 	shrq $8, %rcx # Get jump address
// 	movl %ecx, %r12d # Jump to instruction after if
// 	jmp run_loop
	
// run_instruction_for:
// 	addq $INSTRUCTION_SIZE_FOR, %r12 # Increment intermediate src pointer regardless
// 	cmpb $0, runtime_memory(%r13) # Check if it needs to jump
// 	je run_loop # No jump, just continue with next instruction

// 	shrq $8, %rcx # Get jump address
// 	movl %ecx, %r12d # Jump to instruction after for
// 	jmp run_loop
	
// run_instruction_right:
// 	shrq $8, %rcx # Get ammount to move
// 	addl %ecx, %r13d # Get ammount to move and add to memory pointer
// 	addl $INSTRUCTION_SIZE_RIGHT, %r12d # Increment intermediate src pointer
// 	jmp run_loop

// /*
// 	For the scan instructions, load the next 32 bytes from memory. Then do a saturated subtraction of each 
// 	byte from 1, so that the result may only be 1 if the byte was 0. Now, using the bsf/bsr instruction, we can 
// 	get the index of the first non zero bit, giving us the index of the first byte in memory whose value was 0.
// */
// run_instruction_scan_right_pow2:
// 	movl %r13d, %r15d # Load -memory pointer into %r15
// 	negl %r15d
// 	shrq $8, %rcx # Get memory pointer offset
// 	andq $0xFF, %rcx # Mask out all but the last byte
// 	vmovdqa scan_subtraction_minuends(%rcx), %ymm0 # Load subtraction minuends
// run_instruction_scan_right_pow2_loop:
// 	vmovdqu runtime_memory(%r13d), %ymm1 # Get next 32 bytes
// 	vpsubusb %ymm1, %ymm0, %ymm1 # Subtract each byte from 1
 
// 	# Nothing for the first extraction step, then a vpsrldq to get segment 2, then a vextracti128 to get segment 3 and 4, then a vpsrldq to get segment 4
// 	scan_ymm1_for_0s addl, bsfq, run_instruction_scan_right_pow2_loop, "", "vpsrldq $8, %xmm1, %xmm2", "vextracti128 $1, %ymm1, %xmm3", "vpsrldq $8, %xmm3, %xmm4"
// 	addl $INSTRUCTION_SIZE_SCAN_RIGHT_POW2, %r12d # Increment intermediate src pointer
// 	addl %r13d, %r15d # Add new memory pointer to %r15 to get the movement
// 	jmp run_loop

// run_instruction_scan_left_pow2:
// 	movl %r13d, %r15d # Load -memory pointer into %r15
// 	negl %r15d
// 	shrq $8, %rcx # Get memory pointer offset
// 	andq $0xFF, %rcx # Mask out all but the last byte
// 	vmovdqa scan_subtraction_minuends(%rcx), %ymm0 # Load subtraction minuends
// run_instruction_scan_left_pow2_loop:
// 	vmovdqu runtime_memory - 31(%r13d), %ymm1 # Get next 32 bytes
// 	vpsubusb %ymm1, %ymm0, %ymm4 # Subtract each byte from 1

// 	# For the first extraction step do a vextracti128 and vpsrldq to get segment 3 and 4 of which we get segment 4, then nothing, then get segment 2, and then nothing
// 	scan_ymm1_for_0s subl, bsrq, run_instruction_scan_left_pow2_loop, "vextracti128 $1, %ymm4, %xmm2 ; vpsrldq $8, %xmm2, %xmm1", "", "vpsrldq $8, %xmm4, %xmm3", ""
// 	addl $INSTRUCTION_SIZE_SCAN_LEFT_POW2, %r12d # Increment intermediate src pointer
// 	subl $7, %r13d # Decrement memory pointer by 1, since scan_ymm1_for_0s leaves it off by one in reverse
// 	addl %r13d, %r15d # Add new memory pointer to %r15 to get the movement
// 	jmp run_loop

// run_instruction_scan_right_three:
// 	movl %r13d, %r15d # Load -memory pointer into %r15
// 	negl %r15d
// 	movq $1, %rcx # Init %rcx to 1, represents the offset to get the right subtraction minuends
// run_instruction_scan_right_three_loop:
// 	# Do the mod3 arithmatic
// 	decb %cl # Decrement %rcx by 1 to circle through to the next subtraction minuends
// 	jns 1f # Jump if result was not -1
// 	movb $2, %cl # Reset %rcx to 0
// 1:
// 	vmovdqu 192 + scan_subtraction_minuends(%rcx), %ymm0 # Load subtraction minuends with %rcx
	
// 	vmovdqu runtime_memory(%r13d), %ymm1 # Get next 32 bytes
// 	vpsubusb %ymm1, %ymm0, %ymm1 # Subtract each byte from 1
 
// 	# Nothing for the first extraction step, then a vpsrldq to get segment 2, then a vextracti128 to get segment 3 and 4, then a vpsrldq to get segment 4
// 	scan_ymm1_for_0s addl, bsfq, run_instruction_scan_right_three_loop, "", "vpsrldq $8, %xmm1, %xmm2", "vextracti128 $1, %ymm1, %xmm3", "vpsrldq $8, %xmm3, %xmm4"
// 	addl $INSTRUCTION_SIZE_SCAN_RIGHT_THREE, %r12d # Increment intermediate src pointer
// 	addl %r13d, %r15d # Add new memory pointer to %r15 to get the movement
// 	jmp run_loop

// run_instruction_scan_left_three:
// 	movl %r13d, %r15d # Load -memory pointer into %r15
// 	negl %r15d
// 	movq $4, %rcx # Init %rcx to 1, represents the offset to get the right subtraction minuends
// run_instruction_scan_left_three_loop:
// 	# Do the mod3 arithmatic
// 	subb $2, %cl # Subtract 2 from %rcx to circle through to the next subtraction minuends
// 	jns 1f # Jump if result was not -1 or -2
// 	addb $3, %cl # Add 3 back to make the result positive again
// 1:
// 	vmovdqu 192 + scan_subtraction_minuends(%rcx), %ymm0 # Load subtraction minuends with %rcx
	
// 	vmovdqu runtime_memory - 31(%r13d), %ymm1 # Get next 32 bytes
// 	vpsubusb %ymm1, %ymm0, %ymm4 # Subtract each byte from 1
 
// 	# For the first extraction step do a vextracti128 and vpsrldq to get segment 3 and 4 of which we get segment 4, then nothing, then get segment 2, and then nothing
// 	scan_ymm1_for_0s subl, bsrq, run_instruction_scan_left_three_loop, "vextracti128 $1, %ymm4, %xmm2 ; vpsrldq $8, %xmm2, %xmm1", "", "vpsrldq $8, %xmm4, %xmm3", ""
// 	addl $INSTRUCTION_SIZE_SCAN_LEFT_THREE, %r12d # Increment intermediate src pointer
// 	subl $7, %r13d # Decrement memory pointer by 1, since scan_ymm1_for_0s leaves it off by one in reverse
// 	addl %r13d, %r15d # Add new memory pointer to %r15 to get the movement
// 	jmp run_loop

// run_instruction_scan_manual:
// 	movl %r13d, %r15d # Load -memory pointer into %r15
// 	negl %r15d
// 	shrq $8, %rcx # Get memory pointer offset
// 	subl %ecx, %r13d
// run_instruction_scan_manual_loop:
// 	addl %ecx, %r13d # Loop until a zeroed memory cell is found
// 	cmpb $0, runtime_memory(%r13d)
// 	jne run_instruction_scan_manual_loop

// 	addl %r13d, %r15d # Add new memory pointer to %r15 to get the movement
// 	addl $INSTRUCTION_SIZE_SCAN_MANUAL, %r12d
// 	jmp run_loop

// run_instruction_plus:
// 	movb %ch, %al # Keep ammount to add
// 	shrq $16, %rcx # Get memory pointer offset
// 	addb %al, runtime_memory(%r13d, %ecx) # Add
// 	addq $INSTRUCTION_SIZE_PLUS, %r12 # Increment intermediate src pointer
// 	jmp run_loop
	
// run_instruction_set_zero:
// 	shrq $8, %rcx # Get memory pointer offset
// 	movb $0, runtime_memory(%r13d, %ecx) # Set to 0
// 	addq $INSTRUCTION_SIZE_SET_ZERO, %r12 # Increment intermediate src pointer
// 	jmp run_loop

// run_instruction_load_loop_count:
// 	addq $INSTRUCTION_SIZE_LOAD_LOOP_COUNT, %r12 # Increment intermediate src pointer
// 	cmpb $-1, %ch # Check if add count is 1, then repetition count is x
// 	jne load_loop_count_check_one

// 	shrq $16, %rcx # Get memory pointer offset
// 	movb runtime_memory(%r13d, %ecx), %r15b # Save repetition count
// 	jmp run_loop

// load_loop_count_check_one:
// 	cmpb $1, %ch # Check if add count is 1, then repetition count is 256 - x
// 	jne load_loop_count_non_standard_case

// 	shrq $16, %rcx # Get memory pointer offset
// 	movb runtime_memory(%r13d, %ecx), %r15b # Save repetition count
// 	negb %r15b # Do 256 - x
// 	jmp run_loop

// load_loop_count_non_standard_case:
// 	movq $0, %r15 # Reset repetition count
// 	movq %rcx, %rax # Get memory pointer offset
// 	shrq $16, %rax
// 	movzb runtime_memory(%r13d, %eax), %rax # Get memory input

// 	cmpb $0, %ah # Check if add count is positive or negative
// 	jg load_loop_count_non_standard_case_positive

// 	# Case negative, negate add count
// 	negb %ch # Negate add count
// 	jmp load_loop_count_non_standard_case_loop

// load_loop_count_non_standard_case_positive:
// 	# Case positive, negate input
// 	negb %al # Negate input

// load_loop_count_non_standard_case_loop:
// 	# Divide input by add count
// 	movb $0, %ah # Clear ah
// 	divb %ch # Divide by add count
// 	addb %al, %r15b # Add result to repetition count

// 	# End condition
// 	cmpb $0, %ah # Check if remainder is 0
// 	je run_loop

// 	# There is a remainder, so wrap it around and divide again
// 	movb %ah, %al # Move remainder into al
// 	subb %ch, %al # Subtract add count
// 	incb %r15b # Add 1 to repetition count
// 	jmp load_loop_count_non_standard_case_loop

// run_instruction_mult_add:
// 	movb %ch, %al # Move multiplier into mult register
// 	mulb %r15b # Multiply by repetition count
// 	shrq $16, %rcx # Get memory pointer offset
// 	addb %al, runtime_memory(%r13d, %ecx) # Add
// 	addq $INSTRUCTION_SIZE_MULT_ADD, %r12 # Increment intermediate src pointer
// 	jmp run_loop

// run_instruction_in:
// 	movq %rcx, %r15 # Save input
// 	call getchar
// 	shrq $8, %r15 # Get memory pointer offset
// 	movb %al, runtime_memory(%r13d, %r15d) # Store input into memory
// 	addq $INSTRUCTION_SIZE_IN, %r12 # Increment intermediate src pointer
// 	jmp run_loop

// run_instruction_out:
// 	shrq $8, %rcx # Get memory pointer offset
// 	movb runtime_memory(%r13d, %ecx), %dil # Ouput from memory
// 	call putchar
// 	incq %r14 # Increment output pointer
// 	addq $INSTRUCTION_SIZE_OUT, %r12 # Increment intermediate src pointer

// 	LIMIT_PRINT_COUNT

// 	jmp run_loop
