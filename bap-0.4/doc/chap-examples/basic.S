add %eax, %ebx
shl %cl, %ebx
jc target
jmp elsewhere

target:
	nop

elsewhere:
	nop