initial_ebx:u32 = R_EBX:u32
addr 0x73 @asm "mov    %ebx,%eax"
label pc_0x73
R_EAX:u32 = R_EBX:u32
addr 0x75 @asm "ret    "
label pc_0x75
ra:u32 = mem:?u32[R_ESP:u32, e_little]:u32
R_ESP:u32 = R_ESP:u32 + 4:u32
final_eax:u32 = R_EAX:u32


