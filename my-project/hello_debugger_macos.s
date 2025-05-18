    .data
    .balign 4
_integer:
    .long 100
    .balign 8
_dbl:
    .double 3.5
    .text
    .globl _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    # call a function
    callq _f
    # put some stuff in registers
    movl $0x87654321, %eax
    movsd _dbl(%rip), %xmm0
    # put some stuff on the stack
    movl $0xdeadbeef, -4(%rbp)
    movl $0, -8(%rbp)
    movl $-1, -12(%rbp)
    movl $0xfeedface, -16(%rbp)
    # initialize loop counter
    movl $25, %ecx
L_loop_start:
    # decrement counter
    subl $1, %ecx
    cmpl $0, %ecx
    # jump back to start of loop
    jne L_loop_start
    # return 0
    movl $0, %eax
    movq %rbp, %rsp
    popq %rbp
    ret
    .text
    .globl _f
_f:
    movl $1, %eax
    ret
