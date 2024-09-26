.global main_test
.global intermediate_src_size
.global executed_operations

.equ CLOCK_REALTIME, 0

.data
program_name: .quad 0
program_size: .quad 0
intermediate_src_size: .quad 0
executed_operations: .quad 0
time_nanos: .quad 0

.text

usage_format: .asciz "usage: %s <filename>\n"

# Program name, program size, intermediate src size, executed operations, time in nanos
program_statistics_format: .asciz "\n\n%s\t%lu\t%lu\t%lu\t%lu\n"

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

	# Get the first time stamp
    subq $32, %rsp # Allocate memory for the 2 timespec structs
	movq $228, %rax # clock_gettime
	movq $CLOCK_REALTIME, %rdi
	leaq -32(%rbp), %rsi
	syscall

	/* Timespec struct:
		struct timespec {
			time_t seconds; time_t is 8 byte on 64 bit platform
			long nanos;
		}
	*/

	# Now we're calling you.
	# Good luck.
	movq -16(%rbp), %rdi
	call brainfuck

	# Get the second time stamp
	movq $228, %rax # clock_gettime
	movq $CLOCK_REALTIME, %rdi
	leaq -48(%rbp), %rsi
	call clock_gettime

	# Calculate time taken in nanos
	movq -48(%rbp), %rax # Move end stamp seconds into %rax
	subq -32(%rbp), %rax # Subtract begin stamp seconds from %rax
	movq $1000000000, %rdx # Multiply by 1e9
	mulq %rdx
	addq -40(%rbp), %rax # Add end stamp nanos to %rax
	subq -24(%rbp), %rax # Subtract begin stamp nanon from %rax
	movq %rax, time_nanos # Save into memory

	# Print statistics
	movq $0, %rax
	movq $program_statistics_format, %rdi
	movq program_name, %rsi # Name
	movq program_size, %rdx # Length
	movq intermediate_src_size, %rcx	# Intermediate src length
	movq executed_operations, %r8	# Executed instructions
	movq time_nanos, %r9	# Time in nanos
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
