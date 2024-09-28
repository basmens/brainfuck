.global main_test
.global intermediate_src_size
.global executed_operations
.global compile_time_out

.macro GET_TIME addr_out
	movq \addr_out, %rsi
	movq $228, %rax # clock_gettime
	movq $CLOCK_REALTIME, %rdi
	syscall
.endm

.equ CLOCK_REALTIME, 0

.data
program_name: .quad 0
program_size: .quad 0
intermediate_src_size: .quad 0
executed_operations: .quad 0
time_nanos_compiler: .quad 0
time_nanos_runner: .quad 0

/* Timespec struct:
	struct timespec {
		time_t seconds; time_t is 8 byte on 64 bit platform
		long nanos;
	}
*/
compile_time_out:
	.quad 0 # Second
	.quad 0 # Nanos


.text
.equ TEST_REPETITION_COUNT, 1

usage_format: .asciz "usage: %s <filename>\n"

# Program name, program size, intermediate src size, executed operations, time in nanos
program_statistics_format: .asciz "\n\nDouble word addresses;%s;%lu;%lu;%lu;%lu;%lu\n"

main_test:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp # Used for file storage

	# Make sure we got one argument.
	# The first argument is always the name of our program, so we want a second argument.
	cmp $2, %rdi
	jne wrong_argc

	movq 8(%rsi), %r8
    movq %r8, program_name

	# Read the file with the brainfuck code.
	# 8(%rsi) is argv[1], the path of the file we should read.
	# See read_file.s for details on the read_file subroutine.
	movq 8(%rsi), %rdi
	leaq -8(%rbp), %rsi
	call read_file
	test %rax, %rax
	jz failed
	movq %rax, -16(%rbp)

	# Prepare sleep structure
	pushq $0 # Sleep 0 seconds
	pushq $10000000 # And 10_00_000 nanoseconds

	# Run repetition count times
	movq $TEST_REPETITION_COUNT, -8(%rbp)
test_loop:
	# Call test
	movq -16(%rbp), %rdi
	call test_once

	# Sleep
	movq $162, %rax # Nanosleep call
	leaq -32(%rbp), %rdi
	movq $0, %rsi
	syscall

	# Loop
	decq -8(%rbp)
	jnz test_loop

	# Divide executed operations, time nanos compiler and time nanos runner by repetition count
	movq $TEST_REPETITION_COUNT, %rdi
	movq $0, %rdx
	movq executed_operations, %rax
	divq %rdi
	movq %rax, %r8	# Executed operations
	movq $0, %rdx
	movq time_nanos_compiler, %rax
	divq %rdi
	movq %rax, %r9 # Time nanos compiler
	movq $0, %rdx
	movq time_nanos_runner, %rax
	divq %rdi
	pushq $0 # Keeping stack aligned
	pushq %rax # Time nanos runner

	# Print statistics
	movq $0, %rax
	movq $program_statistics_format, %rdi
	movq program_name, %rsi # Name
	movq program_size, %rdx # Length
	movq intermediate_src_size, %rcx	# Intermediate src length
	call printf

	# Free the buffer allocated by read_file.
	movq -16(%rbp), %rdi
	call free

	# Return success.
	# Unless of course you made us segfault?
	mov $0, %rax
	movq %rbp, %rsp
	popq %rbp
	ret

wrong_argc:
	movq $usage_format, %rdi
	movq (%rsi), %rsi # %rsi still hold argv up to this point
	call printf

failed:
	movq $1, %rax

	movq %rbp, %rsp
	popq %rbp
	ret


test_once:
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	movq %rdi, -24(%rbp)

	# Get the first time stamp
	leaq -16(%rbp), %rdi
	GET_TIME %rdi

	movq -24(%rbp), %rdi
	call brainfuck

	# Get the second time stamp
	leaq -32(%rbp), %rdi
	GET_TIME %rdi

	# Save time compiler
	movq compile_time_out, %rax # Move end stamp seconds into %rax
	subq -16(%rbp), %rax # Subtract begin stamp seconds from %rax
	movq $1000000000, %rdx # Multiply by 1e9
	mulq %rdx
	addq compile_time_out + 8, %rax # Add end stamp nanos to %rax
	subq -8(%rbp), %rax # Subtract begin stamp nanon from %rax
	addq %rax, time_nanos_compiler # Save into memory

	# Save time runner
	movq -32(%rbp), %rax # Move end stamp seconds into %rax
	subq compile_time_out, %rax # Subtract begin stamp seconds from %rax
	movq $1000000000, %rdx # Multiply by 1e9
	mulq %rdx
	addq -24(%rbp), %rax # Add end stamp nanos to %rax
	subq compile_time_out + 8, %rax # Subtract begin stamp nanon from %rax
	addq %rax, time_nanos_runner # Save into memory

	# Reset brainfuck memory
	movq $32048, %rcx
test_reset_loop:
	subq $8, %rcx
	movq $0, intermediate_src(%rcx)
	jnz test_reset_loop

	movq %rbp, %rsp
	popq %rbp
	ret





# Taken from <stdio.h>
.equ SEEK_SET,  0
.equ SEEK_CUR,  1
.equ SEEK_END,  2
.equ EOF,      -1

file_mode: .asciz "r"

# char * read_file(char const * filename, int * read_bytes)
#
# Read the contents of a file into a newly allocated memory buffer.
# The address of the allocated memory buffer is returned and
# read_bytes is set to the number of bytes read.
#
# A null byte is appended after the file contents, but you are
# encouraged to use read_bytes instead of treating the file contents
# as a null terminated string.
#
# Technically, you should call free() on the returned pointer once
# you are done with the buffer, but you are forgiven if you do not.
read_file:
	pushq %rbp
	movq %rsp, %rbp

	# internal stack usage:
	#  -8(%rbp) saved read_bytes pointer
	# -16(%rbp) FILE pointer
	# -24(%rbp) file size
	# -32(%rbp) address of allocated buffer
	subq $32, %rsp

	# Save the read_bytes pointer.
	movq %rsi, -8(%rbp)

	# Open file for reading.
	movq $file_mode, %rsi
	call fopen
	testq %rax, %rax
	jz _read_file_open_failed
	movq %rax, -16(%rbp)

	# Seek to end of file.
	movq %rax, %rdi
	movq $0, %rsi
	movq $SEEK_END, %rdx
	call fseek
	testq %rax, %rax
	jnz _read_file_seek_failed

	# Get current position in file (length of file).
	movq -16(%rbp), %rdi
	call ftell
	cmpq $EOF, %rax
	je _read_file_tell_failed
	movq %rax, -24(%rbp)

    # Store program size
	movq %rax, program_size

	# Seek back to start.
	movq -16(%rbp), %rdi
	movq $0, %rsi
	movq $SEEK_SET, %rdx
	call fseek
	testq %rax, %rax
	jnz _read_file_seek_failed

	# Allocate memory and store pointer.
	# Allocate file_size + 1 for a trailing null byte.
	movq -24(%rbp), %rdi
	incq %rdi
	call malloc
	test %rax, %rax
	jz _read_file_malloc_failed
	movq %rax, -32(%rbp)

	# Read file contents.
	movq %rax, %rdi
	movq $1, %rsi
	movq -24(%rbp), %rdx
	movq -16(%rbp), %rcx
	call fread
	movq -8(%rbp), %rdi
	movq %rax, (%rdi)

	# Add a trailing null byte, just in case.
	movq -32(%rbp), %rdi
	movb $0, (%rdi, %rax)

	# Close file descriptor
	movq -16(%rbp), %rdi
	call fclose

	# Return address of allocated buffer.
	movq -32(%rbp), %rax
	movq %rbp, %rsp
	popq %rbp
	ret

_read_file_malloc_failed:
_read_file_tell_failed:
_read_file_seek_failed:
	# Close file descriptor
	movq -16(%rbp), %rdi
	call fclose

_read_file_open_failed:
	# Set read_bytes to 0 and return null pointer.
	movq -8(%rbp), %rax
	movq $0, (%rax)
	movq $0, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
