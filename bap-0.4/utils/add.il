addr 0x5e @asm "add    %ecx,%edx"
label pc_0x5e
t:u32 = R_EDX:u32
initial_edx:u32 = R_EDX:u32
initial_ecx:u32 = R_ECX:u32
R_EDX:u32 = R_EDX:u32 + R_ECX:u32
R_CF:bool = R_EDX:u32 < t:u32
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EDX:u32 ^ t:u32 ^ R_ECX:u32))
R_OF:bool = high:bool((t:u32 ^ ~R_ECX:u32) & (t:u32 ^ R_EDX:u32))
R_PF:bool =
  ~low:bool(R_EDX:u32 >> 7:u32 ^ R_EDX:u32 >> 6:u32 ^ R_EDX:u32 >> 5:u32 ^
            R_EDX:u32 >> 4:u32 ^ R_EDX:u32 >> 3:u32 ^ R_EDX:u32 >> 2:u32 ^
            R_EDX:u32 >> 1:u32 ^ R_EDX:u32)
R_SF:bool = high:bool(R_EDX:u32)
R_ZF:bool = 0:u32 == R_EDX:u32
addr 0x60 @asm "ret    "
label pc_0x60
ra:u32 = mem:?u32[R_ESP:u32, e_little]:u32
R_ESP:u32 = R_ESP:u32 + 4:u32
final_edx:u32 = R_EDX:u32


