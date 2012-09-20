init_EAX:u32 = R_EAX:u32
init_EBX:u32 = R_EBX:u32
addr 0x61 @asm "mov    %eax,%ecx"
label pc_0x61
R_ECX:u32 = R_EAX:u32
addr 0x63 @asm "add    %ebx,%eax"
label pc_0x63
t:u32 = R_EAX:u32
R_EAX:u32 = R_EAX:u32 + R_EBX:u32
R_CF:bool = R_EAX:u32 < t:u32
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EAX:u32 ^ t:u32 ^ R_EBX:u32))
R_OF:bool = high:bool((t:u32 ^ ~R_EBX:u32) & (t:u32 ^ R_EAX:u32))
R_PF:bool =
  ~low:bool(R_EAX:u32 >> 7:u32 ^ R_EAX:u32 >> 6:u32 ^ R_EAX:u32 >> 5:u32 ^
            R_EAX:u32 >> 4:u32 ^ R_EAX:u32 >> 3:u32 ^ R_EAX:u32 >> 2:u32 ^
            R_EAX:u32 >> 1:u32 ^ R_EAX:u32)
R_SF:bool = high:bool(R_EAX:u32)
R_ZF:bool = 0:u32 == R_EAX:u32
addr 0x65 @asm "xor    %ebx,%ebx"
label pc_0x65
R_EBX:u32 = 0:u32
R_AF:bool = unknown "AF is undefined after xor":bool
R_ZF:bool = true
R_PF:bool = true
R_OF:bool = false
R_CF:bool = false
R_SF:bool = false
addr 0x67 @asm "add    $0x11223344,%ecx"
label pc_0x67
t_69:u32 = R_ECX:u32
R_ECX:u32 = R_ECX:u32 + 0x11223344:u32
R_CF:bool = R_ECX:u32 < t_69:u32
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_ECX:u32 ^ t_69:u32 ^ 0x11223344:u32))
R_OF:bool = high:bool((t_69:u32 ^ 0xeeddccbb:u32) & (t_69:u32 ^ R_ECX:u32))
R_PF:bool =
  ~low:bool(R_ECX:u32 >> 7:u32 ^ R_ECX:u32 >> 6:u32 ^ R_ECX:u32 >> 5:u32 ^
            R_ECX:u32 >> 4:u32 ^ R_ECX:u32 >> 3:u32 ^ R_ECX:u32 >> 2:u32 ^
            R_ECX:u32 >> 1:u32 ^ R_ECX:u32)
R_SF:bool = high:bool(R_ECX:u32)
R_ZF:bool = 0:u32 == R_ECX:u32
addr 0x6d @asm "sete   %bl"
label pc_0x6d
R_EBX:u32 = R_EBX:u32 & 0xffffff00:u32 | pad:u32(R_ZF:bool)
addr 0x70 @asm "add    %ebx,%eax"
label pc_0x70
t_70:u32 = R_EAX:u32
R_EAX:u32 = R_EAX:u32 + R_EBX:u32
R_CF:bool = R_EAX:u32 < t_70:u32
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EAX:u32 ^ t_70:u32 ^ R_EBX:u32))
R_OF:bool = high:bool((t_70:u32 ^ ~R_EBX:u32) & (t_70:u32 ^ R_EAX:u32))
R_PF:bool =
  ~low:bool(R_EAX:u32 >> 7:u32 ^ R_EAX:u32 >> 6:u32 ^ R_EAX:u32 >> 5:u32 ^
            R_EAX:u32 >> 4:u32 ^ R_EAX:u32 >> 3:u32 ^ R_EAX:u32 >> 2:u32 ^
            R_EAX:u32 >> 1:u32 ^ R_EAX:u32)
R_SF:bool = high:bool(R_EAX:u32)
R_ZF:bool = 0:u32 == R_EAX:u32

final_EAX:u32 = R_EAX:u32
