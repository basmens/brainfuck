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
output_buffer: .skip 30000, 0

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
	push %r12 # -8
	push %r13 # -24 Becomes executable source write pointer
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
	push %r13 # -48 Save executable source block

	# Write 'xor %r13, %r13' into executable memory as first instruction
	movl $0x00ED314D, (%r13)
	addq $3, %r13 # Increment executable source block

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
	movq $read_loop, %r15 # Last read instruction
	movq $0, %rbx # The instruction repetition counter
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

compile_return:
	# Write ret instruction
	movb $0xC3, (%r13)

	# Pop final bracket frame
	movq %rbp, %rsp
	popq %rbp

	SET_INTERMEDIATE_SRC_SIZE_STAT # Comment out above
	DECOMPILER # Comment out above
	GET_TIME # Comment out above

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
# Write instruction
##############################################################################################################################################
.equ INSTRUCTION_SIZE_RIGHT, 7
/*
	#0 41 81 C4 .skip 4 (amount)		addl $amount, %r12
*/
.macro write_instruction_right amount
	movl $0x00C48141, (%r13)
	movl \amount, 3(%r13)
	addq $INSTRUCTION_SIZE_RIGHT, %r13
.endm

.equ INSTRUCTION_SIZE_PLUS, 9
/*
	#0 41 80 84 24 .skip 4 (address) .skip 1 (amount)		addq amount, address(%r12)
*/
.macro write_instruction_plus amount, address
	movq $0x24848041, (%r13)
	movl \address, 4(%r13)
	movb \amount, 8(%r13)
	addq $INSTRUCTION_SIZE_PLUS, %r13
.endm

.equ INSTRUCTION_SIZE_IN, 38
/*
    #0  48 C7 C0 01 00 00 00				movq $1, %rax
    #7  48 89 C7							movq %rax, %rdi
    #10 48 C7 C6 .long output_buffer		movq $output_buffer, %rsi
    #17 4C 89 EA							movq %r13, %rdx
    #20 0F 05                               syscall
	#22 4D 31 ED							xorq %r13, %r13

	#25 E8 .skip 4 (address offset)			call getchar
	#30 41 88 84 24 .skip 4 (address)		movb %al, address(%r12)
*/
.macro write_instruction_in address
	# Print everything in the output buffer to this point
	movl $0x01C0C748, (%r13)
	movl $0x48000000, 4(%r13)
	movl $0xC748C789, 8(%r13)
	movb $0xC6, 12(%r13)
	movl $output_buffer, 13(%r13)
	movl $0x0FEA894C, 17(%r13)
	movl $0xED314D05, 21(%r13)

	movb $0xE8, 25(%r13)
	movl $0x24848841, 30(%r13)
	movl \address, 34(%r13)

	# Calculate address offset
	movl $getchar - 30, %eax # Get target address minus index of first byte after call
	subl %r13d, %eax # Get offset from start of executable memory
	movl %eax, 26(%r13) # Insert address offset into instruction
	addq $INSTRUCTION_SIZE_IN, %r13
.endm

.equ INSTRUCTION_SIZE_OUT, 18
/*
	#0  41 8A 84 24 .skip 4 (address)		movb address(%r12), %al
	#8  41 88 85 .long output_buffer		movb %al, output_buffer(%r13)
	#15 49 FF C5							incq %r13
*/
.macro write_instruction_out address
	movl $0x24848A41, (%r13)
	movl \address, 4(%r13)
	movl $0x00858841, 8(%r13)
	movl $output_buffer, 11(%r13)
	movl $0x00C5FF49, 15(%r13)
	addq $INSTRUCTION_SIZE_OUT, %r13
.endm

.equ INSTRUCTION_SIZE_IF, 11
/*
	#0 41 80 3C 24 00					cmpb $0, (%r12)
	#5 0F 84 .skip 4 (address offset)	je(long jump) address
*/
.macro write_instruction_if
	movl $0x243C8041, (%r13)
	movl $0x00840F00, 4(%r13)
	addq $INSTRUCTION_SIZE_IF, %r13
.endm

.equ INSTRUCTION_SIZE_FOR, 11
/*
	#0 41 80 3C 24 00					cmpb $0, (%r12)
	#5 0F 85 .skip 4 (address offset)	jne(long jump) address
*/
.macro write_instruction_for
	movl $0x243C8041, (%r13)
	movl $0x00850F00, 4(%r13)
	addq $INSTRUCTION_SIZE_FOR, %r13
.endm

.equ INSTRUCTION_SIZE_SET, 9
/*
	#0 41 C6 84 24 .skip 4 (address). skip 1 (amount)	movb $amount, address(%r12)
*/
.macro write_instruction_set amount address
	movl $0x2484C641, (%r13)
	movl \address, 4(%r13)
	movb $\amount, 8(%r13)
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
/*
	#0 45 8A B4 24 .skip 4 (address)		movb address(%r12), %r14b
*/
.macro write_instruction_load address
	movl $0x24B48A45, (%r13)
	movl \address, 4(%r13)
	addq $INSTRUCTION_SIZE_LOAD, %r13
.endm

.equ INSTRUCTION_SIZE_NEG_LOADED, 3
/*
	#8 41 F6 DE								negb %r14b
*/
.macro write_instruction_neg_loaded address
	movl $0x00DEF641, 8(%r13)
	addq $INSTRUCTION_SIZE_NEG_LOADED, %r13
.endm

.equ INSTRUCTION_SIZE_LOAD_POW2, 26
/*
	#0  41 F6 C6 .skip 1 (test_mask)		testb $test_mask, %r14b
	#4  74 10								jz 1f
	#6  48 C7 C0 3C 00 00 00				movq $60, %rax (Exit)
	#13 48 C7 C7 0A 00 00 00				movq $10, %rdi
	#20 0F 05                               syscall
	#22 41 C0 EE .skip 1 (shift_count)		1: shrb $shift_count, %r14b
*/
.macro write_instruction_load_pow2 add_count
	movl $0x00C6F641, (%r13)
	decb \add_count # Get test mask
	movb \add_count, 3(%r13)
	incb \add_count # Restore
	movl $0xC7481074, 4(%r13)
	movl $0x00003CC0, 8(%r13)
	movl $0xC7C74800, 12(%r13)
	movl $0x0000000A, 16(%r13)
	movl $0xC041050F, 20(%r13)
	movb $0xEE, 24(%r13)
	movzb \add_count, %r8w
	bsfw %r8w, %r8w # Get shift count
	movb %r8b, 25(%r13)
	addq $INSTRUCTION_SIZE_LOAD_POW2, %r13
.endm

.equ INSTRUCTION_SIZE_LOAD_LOOP_COUNT_SCAN, 29
/*
	#0  44 88 F0							movb %r14b, %al
	#3  B1 .skip 1 (add_count)				movb $add_count, %cl
	#5  45 30 F6							xorb %r14b, %r14b
	#8  B4 00								1: movb $0, %ah
	#10 F6 F1								divb %cl
	#12 41 00 C6							addb %al, %r14b
	#15 80 FC 00							cmpb $0, %ah
	#18 74 09								je 1f
	#20 88 E0								movb %ah, %al
	#22 28 C8								subb %cl, %al
	#24 41 FE C6							incb %r14b
	#27 EB EB								jmp 1b 1:
*/
.macro write_instruction_load_loop_count_scan add_count
	movl $0xB1F08844, (%r13)
	movb \add_count, 4(%r13)
	movl $0xB4F63045, 5(%r13)
	movl $0x41F1F600, 9(%r13)
	movl $0xFC80C600, 13(%r13)
	movl $0x88097400, 17(%r13)
	movl $0x41C828E0, 21(%r13)
	movl $0xEBEBC6FE, 25(%r13)
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
/*
	#0 45 00 B4 24 .skip 4 (address)			addb %r14b, address(%r12)
*/
.macro write_instruction_add address
	movl $0x24B40045, (%r13)
	movl \address, 4(%r13)
	addq $INSTRUCTION_SIZE_ADD, %r13
.endm

.equ INSTRUCTION_SIZE_MULT_ADD_POW2, 14
/*
	#0 44 88 F0									movb %r14b, %al
	#3 C0 E0 .skip 1 (shift_count)				shlb $shift_count, %al
	#6 41 00 84 24 .skip 4 (address)			addb %al, address(%r12)
*/
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
/*
	#0 B0 .skip 1 (amount)						movb $amount, %al
	#2 41 F6 E6									mulb %r14b
	#5 41 00 84 24 .skip 4 (address)			addb %al, address(%r12)
*/
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

##############################################################################################################################################
# Compile
##############################################################################################################################################
.macro compile_pop_mem_movement
	movl -12(%rbp), %eax # Get memory pointer offset from bracket frame
	cmpl $0, %eax # If memory pointer offset is 0, skip
	je 1f

	# Memory pointer is not set to zero. That will be done by compile_for if it
	# is confirmed that the loop cannot be optimized.
	write_instruction_right %eax # Write instruction
1:
.endm

compile_if:
	compile_pop_mem_movement

	pushq %rbp
	movq %rsp, %rbp
	pushq %r13 # Push new empty bracket frame
	pushq $0
	write_instruction_if

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
	write_instruction_plus %dl %eax
	orl $LOOP_CONTAINS_PLUS, -16(%rbp) # Loop contains a plus instruction
	jmp read_loop

compile_in:
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_instruction_in %eax
	orl $LOOP_CONTAINS_IO, -16(%rbp) # Loop contains a in instruction
	jmp read_loop

compile_out:
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_instruction_out %eax
	orl $LOOP_CONTAINS_IO, -16(%rbp) # Loop contains a out instruction
	jmp read_loop


##############################################################################################################################################
# Compile loops
##############################################################################################################################################
.macro remove_right_instruction_via_r8 src_pointer
	movl -INSTRUCTION_SIZE_RIGHT(\src_pointer), %r8d # Check if previous instruction was a move instruction
	andl $0x00FFFFFF, %r8d # And out the first bit of the amount so that only the opcode bits are there
	cmpl $0x00C48141, %r8d
	jne 1f
	subq $INSTRUCTION_SIZE_RIGHT, \src_pointer
1:
.endm


################################## None ##################################
compile_for_no_optimizations:
	compile_pop_mem_movement

	write_instruction_for

	movq -8(%rbp), %rdx # Get address of if instruction
	addq $INSTRUCTION_SIZE_IF, %rdx # Move address to instruction after if
	write_jump_offset %r13, -4, %rdx
	write_jump_offset %rdx, -4, %r13

	movq %rbp, %rsp # Pop bracket frame
	popq %rbp
	movl $0, -12(%rbp) # Reset memory pointer to match the movement before the if

	orl $LOOP_CONTAINS_LOOP, -16(%rbp) # Loop contains a inner loop
	jmp read_loop


################################## Mult ##################################
compile_loop_mult:
	# Check if it is a set zero instruction
	movq %r13, %rdx # Calculate total amount writen by taking the current write pointer
	movq -8(%rbp), %rax # Subtract the address of the if instruction whilst keeping a
	subq %rax, %rdx # copy of it in %rax
	subq $INSTRUCTION_SIZE_IF, %rdx # And subtract the size of the if instruction
	cmpl $INSTRUCTION_SIZE_PLUS, %edx
	je compile_loop_set_zero

	# Pop bracket frame
	movq %rbp, %rsp
	popq %rbp

	/*
		We first read out all the plus instruction onto the stack, unless the plus instruction is a source add instruction,
		then we keep track of it in %rcx. After reading, we write back the instructions as a multiplications, with a
		load loop count instruction beforehand and a set zero instruction after. %rdi will be used as index through the loop.
		And %rdx will be used to store the original the stack pointer. Meanwhile, %rax will just preserve the address of the
		if instruction to be used later.
	*/

	# Loop through all the plus instructions
	movq %rax, %rdi # Get address of first plus instruction's address
	addq $INSTRUCTION_SIZE_IF + 4, %rdi
	movb $0, %cl # Set source add count to 0
	movq %rsp, %rdx # Preserve stack pointer in %rdx
compile_mult_read_loop:
	# End condition
	cmpq %r13, %rdi
	ja compile_mult_read_loop_end

	# Load address and amount of the plus instruction
	pushq (%rdi) # Push the address and amount of the plus instruction to the stack
	addq $INSTRUCTION_SIZE_PLUS, %rdi # Increment %rdi by INSTRUCTION_SIZE_PLUS
	cmpl $0, (%rsp) # Check if it is a source add instruction
	jne compile_mult_read_loop

	# Increment source add count
	addb 4(%rsp), %cl # Increment source add count
	addq $8, %rsp # Pop the address and amount of the plus instruction
	jmp compile_mult_read_loop
compile_mult_read_loop_end:

	# Move write pointer back
	movq %rax, %r13
	remove_right_instruction_via_r8 %r13

	# Write load loop count instruction
	movl -12(%rbp), %eax # Get offset from memory pointer
	write_load_loop_count %cl, %eax

	# Write mult add instructions
compile_mult_write_loop:
	# End condition
	cmpq %rdx, %rsp
	je compile_mult_write_loop_end

	popq %rdi # Get address and amount of plus instruction
	movq %rdi, %rcx # Copy over to %rcx
	shrq $32, %rcx # Extract amount
	addl %eax, %edi # Add memory pointer offset of previoues bracket frame
	write_mult_add %cl, %edi
	jmp compile_mult_write_loop
compile_mult_write_loop_end:

	# Write set zero instruction
	write_instruction_set 0, %eax
	orl $LOOP_CONTAINS_SET, -16(%rbp) # Loop contains a set zero
	orl $LOOP_CONTAINS_MULT, -16(%rbp) # Loop contains a multiplication
	jmp read_loop


################################## Set zero ##################################
compile_loop_set_zero:
	# Pop bracket frame
	movq %rbp, %rsp
	popq %rbp

	# Calculate offset
	movl -INSTRUCTION_SIZE_PLUS + 4(%r13), %eax # Get address offset of plus instruction
	addl -12(%rbp), %eax # Add memory pointer offset

	# Move back the write pointer
	subq $INSTRUCTION_SIZE_IF + INSTRUCTION_SIZE_PLUS, %r13
	remove_right_instruction_via_r8 %r13

	# Write set zero
	write_instruction_set 0 %eax
	orl $LOOP_CONTAINS_SET, -16(%rbp) # Loop contains a set zero
	jmp read_loop




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
	.quad compile_for_no_optimizations # 0x20
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
