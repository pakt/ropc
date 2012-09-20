goal:bool = false
addr 0x0 @asm "add    %eax,%ebx"
label pc_0x0
t:u32 = R_EBX:u32
R_EBX:u32 = R_EBX:u32 + R_EAX:u32
R_CF:bool = R_EBX:u32 < t:u32
R_AF:bool = 0x10:u32 == (0x10:u32 & (R_EBX:u32 ^ t:u32 ^ R_EAX:u32))
R_OF:bool = high:bool((t:u32 ^ ~R_EAX:u32) & (t:u32 ^ R_EBX:u32))
R_PF:bool =
  ~low:bool(R_EBX:u32 >> 7:u32 ^ R_EBX:u32 >> 6:u32 ^ R_EBX:u32 >> 5:u32 ^
            R_EBX:u32 >> 4:u32 ^ R_EBX:u32 >> 3:u32 ^ R_EBX:u32 >> 2:u32 ^
            R_EBX:u32 >> 1:u32 ^ R_EBX:u32)
R_SF:bool = high:bool(R_EBX:u32)
R_ZF:bool = 0:u32 == R_EBX:u32
addr 0x2 @asm "shl    %cl,%ebx"
label pc_0x2
tmpDEST:u32 = R_EBX:u32
t1:u32 = R_EBX:u32 >> 0x20:u32 - (R_ECX:u32 & 0x1f:u32)
R_CF:bool =
  if (R_ECX:u32 & 0x1f:u32) == 0:u32 then R_CF:bool else low:bool(t1:u32)
R_EBX:u32 = R_EBX:u32 << (R_ECX:u32 & 0x1f:u32)
R_OF:bool =
  if (R_ECX:u32 & 0x1f:u32) == 0:u32 then R_OF:bool else
  if (R_ECX:u32 & 0x1f:u32) == 1:u32 then high:bool(R_EBX:u32) ^ R_CF:bool
  else unknown "OF <- undefined":bool
R_SF:bool =
  if (R_ECX:u32 & 0x1f:u32) == 0:u32 then R_SF:bool else high:bool(R_EBX:u32)
R_ZF:bool =
  if (R_ECX:u32 & 0x1f:u32) == 0:u32 then R_ZF:bool else 0:u32 == R_EBX:u32
R_PF:bool =
  if (R_ECX:u32 & 0x1f:u32) == 0:u32 then R_PF:bool else
  ~low:bool(R_EBX:u32 >> 7:u32 ^ R_EBX:u32 >> 6:u32 ^ R_EBX:u32 >> 5:u32 ^
            R_EBX:u32 >> 4:u32 ^ R_EBX:u32 >> 3:u32 ^ R_EBX:u32 >> 2:u32 ^
            R_EBX:u32 >> 1:u32 ^ R_EBX:u32)
R_AF:bool =
  if (R_ECX:u32 & 0x1f:u32) == 0:u32 then R_AF:bool else
  unknown "AF undefined after shift":bool
addr 0x4 @asm "jb     0x0000000000000008"
label pc_0x4
cjmp R_CF:bool, 8:u32, "nocjmp0"
label nocjmp0
addr 0x6 @asm "jmp    0x0000000000000009"
label pc_0x6
jmp 9:u32
addr 0x8 @asm "nop"
label pc_0x8
goal:bool = true
addr 0x9 @asm "nop"
label pc_0x9


