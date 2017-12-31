	.section .rodata

#define PAGE_SIZE   4096
#define NEXT_OFFSET 4080
#define PREV_OFFSET 4088
#define SYMBOLS8    4080
#define SYMBOLS16   2040
#define SYMBOLS32   1020
#define SYMBOLS64    510

        .globl turingmachine_setup, turingmachine_setup_length, turingmachine_setup_mask
turingmachine_setup:
	//%rdi - (tape)
	//%rsi - (pos)
	//%rdx - (state)
	//%rcx - count
	push %rbx
	mov (%rdi), %rbx
	mov (%rsi), %r8
	mov (%rdx), %r9
	mov $0x7fffffffffffffff, %r10
.Lturingmachine_setup.mask:
	//%rbx - tape
	//%r8 - pos
	//%r9 - state
	//%r10 - mask
	.size turingmachine_setup, . - turingmachine_setup
turingmachine_setup_length:
	.int . - turingmachine_setup
turingmachine_setup_mask:
	.int .Lturingmachine_setup.mask - turingmachine_setup - 8

turingmachine_loop8:
	mov (%rbx, %r8, 1), %al
	and %r10, %rax
	or %r9, %rax
	.size turingmachine_loop8, . - turingmachine_loop8
turingmachine_loop8_length:
	.int . - turingmachine_loop8

turingmachine_loop16:
	mov (%rbx, %r8, 2), %ax
	.size turingmachine_loop16, . - turingmachine_loop16
turingmachine_loop16_length:
	.int . - turingmachine_loop16

turingmachine_loop32:
	mov (%rbx, %r8, 4), %eax
	.size turingmachine_loop32, . - turingmachine_loop32
turingmachine_loop32_length:
	.int . - turingmachine_loop32

turingmachine_loop64:
	mov (%rbx, %r8, 8), %rax
	.size turingmachine_loop64, . - turingmachine_loop64
turingmachine_loop64_length:
	.int . - turingmachine_loop64

turingmachine_tail:
	dec %rcx
	jz .Lturingmachine_tail.jmp
	jmp *-0x80000000(%rip)
.Lturingmachine_tail.jmp:
	mov %rcx, %rax
	mov %r8, (%rsi)
	mov %r9, (%rdx)
	pop %rbx
	ret
	.size turingmachine_tail, . - turingmachine_tail
turingmachine_tail_length:
	.int . - turingmachine_tail
turingmachine_tail_jmp:
	.int .Lturingmachine_tail.jmp - turingmachine_tail
